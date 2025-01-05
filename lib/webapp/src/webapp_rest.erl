-module(webapp_rest).
-export([start_link/0]).
%% rester_http_server callbacks
-export([init/2, info/3, close/2, error/3, http_request/4]).
-export([delete_all_stale_timestamps/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("rester/include/rester.hrl").
-include_lib("rester/include/rester_http.hrl").
-include_lib("rester/include/rester_socket.hrl").
-include_lib("db/include/db.hrl").
-include("webapp_auth.hrl").

-define(PORTAL_CACHE, portal_cache).
-define(PORTAL_TIMEOUT, 5 * 60). % 5 minutes

-define(CHALLENGE_CACHE, challenge_cache).
-define(CHALLENGE_TIMEOUT, 5 * 60). % 5 minutes

-type(timestamp() :: integer()).

-record(state, {
                subscriptions = #{} ::
                  #{rester_socket() =>
                        {Request :: term(), db_serv:subscription_id()}}
               }).

-record(portal_cache_entry, {
                             ip_address :: inet:ip_address(),
                             mac_address :: db_user_serv:mac_address(),
                             timestamp :: timestamp()
                            }).

-record(challenge_cache_entry, {
                                username :: db_user_serv:username(),
                                challenge :: webapp_auth:challenge(),
                                timestamp :: timestamp()
                               }).

%%
%% Exported: start_link
%%

start_link() ->
    ok = webapp_dnsmasq:clear_all_mac_addresses(),
    Options =
	[{request_module, ?MODULE},
         {verify, verify_none},
         {cacerts, []},
         {certfile, filename:join([code:priv_dir(webapp), "cert.pem"])},
	 {nodelay, true},
	 {reuseaddr, true}],
    %% Create a named table for the portal cache
    ?PORTAL_CACHE = ets:new(?PORTAL_CACHE,
                            [{keypos, #portal_cache_entry.ip_address},
                             public, named_table]),
    {ok, _} = timer:apply_interval(?PORTAL_TIMEOUT * 1000,
                                   ?MODULE, delete_all_stale_timestamps, []),
    %% Create a named table for the challenge cache
    ?CHALLENGE_CACHE = ets:new(?CHALLENGE_CACHE,
                               [{keypos, #challenge_cache_entry.username},
                                public, named_table]),
    ?log_info("Database REST API has been started"),
    HttpPort = application:get_env(webapp, http_port, 80),
    {ok, _} = rester_http_server:start_link(HttpPort, Options),
    case application:get_env(webapp, https_port, undefined) of
	undefined ->
	    ignore;
	HttpPort ->
	    ok;
	HttpsPort ->
	    rester_http_server:start_link(HttpsPort, Options)
    end.

%%
%% Exported: init callback
%%

init(_Socket, Options) ->
    ?log_info("init: ~p", [Options]),
    {ok, #state{}}.

%%
%% Exported: info callback
%%

info(Socket, {subscription_change, SubscriptionId, PostId}, State) ->
    ?log_info("Post ~s has been changed", [PostId]),
    case maps:get(Socket, State#state.subscriptions, not_found) of
        not_found ->
            {ok, State};
        {Request, SubscriptionId} ->
            rest_util:response(Socket, Request, {ok, {format, PostId}}),
            UpdatedState =
                State#state{
                  subscriptions =
                      maps:remove(Socket, State#state.subscriptions)},
            {ok, UpdatedState};
        SpuriousSubscriptionId ->
            ?log_error("Spurious subscription id ~p\n",
                       [SpuriousSubscriptionId]),
            {ok, State}
    end;
info(_Socket, Info, State) ->
    ?log_info("info: ~p\n", [{Info, State}]),
    {ok, State}.

%%
%% Exported: close callback
%%

close(_Socket, State) ->
    ?log_info("close: ~p\n", [State]),
    {ok, State}.

%%
%% Exported: error callback
%%

error(_Socket, Error, State) ->
    ?log_info("error: ~p", [{Error, State}]),
    {stop, normal, State}.

%%
%% Exported: http_request callback
%%

http_request(Socket, Request, Body, State) ->
    ?log_info("Request = ~s, Headers = ~s, Body = ~p",
              [rester_http:format_request(Request),
               rester_http:format_hdr(Request#http_request.headers),
               Body]),
    try http_request_(Socket, Request, Body, State) of
	Result ->
            Result
    catch
	_Class:Reason:StackTrace ->
	    ?log_error("http_request crashed: ~p\n~p\n",
                       [Reason, StackTrace]),
	    erlang:error(Reason)
    end.

http_request_(Socket, Request, Body, State) ->
    case Request#http_request.method of
	'GET' ->
	    http_get(Socket, Request, Body, State);
	'POST' ->
	    http_post(Socket, Request, Body, State);
	_ ->
	    rest_util:response(Socket, Request, {error, not_allowed})
    end.

%%
%% HTTP GET
%%

http_get(Socket, Request, Body, State) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["versions"] ->
	    Object = json:encode([<<"v1">>]),
	    rester_http_server:response_r(Socket, Request, 200, "OK", Object,
                                          [{content_type, "application/json"}]);
	["v1"|Tokens] ->
	    http_get(Socket, Request, Url, Tokens, Body, State, v1);
	Tokens ->
	    http_get(Socket, Request, Url, Tokens,  Body, State, v1)
    end.

http_get(Socket, Request, Url, Tokens, _Body, _State, v1) ->
    Headers = Request#http_request.headers,
    case Tokens of
        %% On Ubuntu Core, the network manager will check for connectivity
        _ when Headers#http_chdr.host == "connectivity-check.ubuntu.com." orelse
               Headers#http_chdr.host == "connectivity-check.ubuntu.com" ->
            ?log_info("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, ubuntu);
        %% In Firefox, the browser will check for a captive portal
        %% https://support.mozilla.org/en-US/kb/captive-portal
        ["canonical.html" = Page]
          when Headers#http_chdr.host == "detectportal.firefox.com" ->
            ?log_info("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, Page);
        ["success.txt" = Page]
          when Headers#http_chdr.host == "detectportal.firefox.com" ->
            ?log_info("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, Page);
        %% Apple devices will check for a captive portal
        ["hotspot-detect.html" = Page] ->
            ?log_info("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, Page);
        %% Android devices will check for a captive portal
        ["generate_204" = Page] ->
            ?log_info("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, Page);
        ["gen_204" = Page] ->
            ?log_info("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, Page);
        %% Captive portal acks are generated by loader.js
        ["captive_portal_ack"] ->
            ?log_info("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            {ok, MacAddress} = get_mac_address(Socket),
            ok = webapp_dnsmasq:set_post_login_mac_address(MacAddress),
            {ok, {IpAddress, _Port}} = rester_socket:peername(Socket),
            {ok, MacAddress} = get_mac_address(Socket),
            PortalCacheEntry =
                #portal_cache_entry{ip_address = IpAddress,
                                    mac_address = MacAddress,
                                    timestamp = timestamp()},
            true = ets:insert(?PORTAL_CACHE, PortalCacheEntry),
            rester_http_server:response_r(
              Socket, Request, 204, "OK", "", no_cache_headers());
        %% Bespoke API
        ["list_top_posts"] ->
            true = update_portal_cache_entry(Socket),
            Posts = db_serv:list_top_posts(),
            PayloadJsonTerm = lists:map(fun(Post) ->
                                                post_to_json_term(Post)
                                        end, Posts),
            rest_util:response(Socket, Request,
                               {ok, {format, PayloadJsonTerm}});
        ["auto_login"] ->
            case filelib:is_regular("/var/tmp/bespoke/bootstrap") of
                true ->
                    rester_http_server:response_r(
                      Socket, Request, 302, "Found", "",
                      [{location, "/bootstrap.html"}|no_cache_headers()]);
                false ->
                    {ok, MacAddress} = get_mac_address(Socket),
                    User = db_user_serv:get_user_from_mac_address(MacAddress),
                    PayloadJsonTerm =
                        #{<<"noPassword">> => true,
                          <<"userId">> => User#user.id,
                          <<"username">> => User#user.name,
                          <<"sessionId">> =>
                              base64:encode(User#user.session_id)},
                    case User#user.password_hash of
                        not_set ->
                            rest_util:response(Socket, Request,
                                               {ok, {format, PayloadJsonTerm}});
                        _ ->
                            UpdatedPayloadJsonTerm =
                                maps:put(<<"noPassword">>, false,
                                         PayloadJsonTerm),
                            rest_util:response(
                              Socket, Request,
                              {ok, {format, UpdatedPayloadJsonTerm}})
                    end
            end;
        %% Act as static web server
	Tokens ->
            true = update_portal_cache_entry(Socket),
            UriPath =
                case Tokens of
                    [] ->
                        "loader.html";
                    _ ->
                        Url#url.path
                end,
            AbsFilename =
                filename:join(
                  [filename:absname(code:priv_dir(webapp)), "docroot",
                   tl(UriPath)]),
            case filelib:is_regular(AbsFilename) of
                true ->
                    rester_http_server:response_r(
                      Socket, Request, 200, "OK", {file, AbsFilename},
                      [{content_type, {url, UriPath}}]);
                false ->
                    rest_util:response(Socket, Request, {error, not_found})
            end
    end.

%%
%% HTTP POST
%%

http_post(Socket, Request, Body, State) ->
    true = update_portal_cache_entry(Socket),
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["v1" | Tokens] ->
	    http_post(Socket, Request, Url, Tokens, Body, State, v1);
	Tokens ->
	    http_post(Socket, Request, Url, Tokens, Body, State, v1)
    end.

http_post(Socket, Request, _Url, Tokens, Body, State, v1) ->
    case Tokens of
        ["bootstrap"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                JsonTerm ->
                    case json_term_to_bootstrap(JsonTerm) of
                        {ok, SSID} ->
                            ok = file:delete("/var/tmp/bespoke/bootstrap"),
                            ok = change_ssid(SSID),
                            %% Purge the portal cache
                            true = ets:delete_all_objects(?PORTAL_CACHE),
                            rest_util:response(Socket, Request, ok_204);
                        {error, invalid} ->
                            rest_util:response(Socket, Request, {error, badarg})
                    end
            end;
        ["generate_challenge"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                Username when is_binary(Username) ->
                    case db_user_serv:get_user_from_username(Username) of
                        {ok, #user{password_salt = PasswordSalt}} ->
                            ok;
                        {error, not_found} ->
                            PasswordSalt = crypto:strong_rand_bytes(
                                             ?CRYPTO_PWHASH_SALTBYTES)
                    end,
                    Challenge = webapp_auth:generate_challenge(),
                    PayloadJsonTerm =
                        #{<<"passwordSalt">> => base64:encode(PasswordSalt),
                          <<"challenge">> => base64:encode(Challenge)},
                    true = add_challenge_to_cache(Username, Challenge),
                    rest_util:response(Socket, Request,
                                       {ok, {format, PayloadJsonTerm}});
                _ ->
                    rest_util:response(Socket, Request, {error, badarg})
            end;
        ["login"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                JsonTerm ->
                    case json_term_to_login(JsonTerm) of
                        {ok, Username, ClientResponse} ->
                            login(Socket, Request, Username, ClientResponse);
                        {error, invalid} ->
                            rest_util:response(Socket, Request, {error, badarg})
                    end
            end;
        ["switch_user"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                JsonTerm ->
                    case json_term_to_switch_user(JsonTerm) of
                        {ok, Username, PasswordSalt, PasswordHash,
                         ClientResponse} ->
                            switch_user(Socket, Request, Username, PasswordSalt,
                                        PasswordHash, ClientResponse);
                        {error, invalid} ->
                            rest_util:response(Socket, Request, {error, badarg})
                    end
            end;
        ["change_password"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                JsonTerm ->
                    case json_term_to_change_password(JsonTerm) of
                        {ok, PasswordSalt, PasswordHash} ->
                            change_password(Socket, Request, PasswordSalt,
                                            PasswordHash);
                        {error, invalid} ->
                            rest_util:response(Socket, Request, {error, badarg})
                    end
            end;
        ["lookup_posts"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                PostIds when is_list(PostIds) ->
                    case lists:all(fun(PostId) when is_binary(PostId) ->
                                           true;
                                      (_) ->
                                           false
                                   end, PostIds) of
                        true ->
                            Posts = db_serv:lookup_posts(PostIds),
                            PayloadJsonTerm =
                                lists:map(fun(Post) ->
                                                  post_to_json_term(Post)
                                          end, Posts),
                            rest_util:response(
                              Socket, Request, {ok, {format, PayloadJsonTerm}});
                        false ->
                            rest_util:response(Socket, Request, {error, badarg})
                    end;
                _ ->
                    rest_util:response(Socket, Request, {error, badarg})
            end;
        ["lookup_recursive_posts"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                PostIds when is_list(PostIds) ->
                    case lists:all(fun(PostId) when is_binary(PostId) ->
                                           true;
                                      (_) ->
                                           false
                                   end, PostIds) of
                        true ->
                            Posts =
                                db_serv:lookup_posts(PostIds, recursive),
                            PayloadJsonTerm =
                                lists:map(fun(Post) ->
                                                  post_to_json_term(Post)
                                          end, Posts),
                            rest_util:response(
                              Socket, Request, {ok, {format, PayloadJsonTerm}});
                        false ->
                            rest_util:response(Socket, Request, {error, badarg})
                    end;
                _ ->
                    rest_util:response(Socket, Request, {error, badarg})
            end;
        ["lookup_recursive_post_ids"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                PostIds when is_list(PostIds) ->
                    case lists:all(fun(PostId) when is_binary(PostId) ->
                                           true;
                                      (_) ->
                                           false
                                   end, PostIds) of
                        true ->
                            PayloadJsonTerm =
                                db_serv:lookup_post_ids(PostIds, recursive),
                            rest_util:response(
                              Socket, Request, {ok, {format, PayloadJsonTerm}});
                        false ->
                            rest_util:response(Socket, Request, {error, badarg})
                    end;
                _ ->
                    rest_util:response(Socket, Request, {error, badarg})
            end;
        ["insert_post"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                JsonTerm ->
                    {ok, #{<<"sessionId">> := SessionId}} =
                        get_bespoke_cookie(Request),
                    case db_user_serv:get_user_from_session_id(
                           base64:decode(SessionId)) of
                        {ok, #user{name = Username}} ->
                            case json_term_to_post(JsonTerm, Username) of
                                {ok, Post} ->
                                    case db_serv:insert_post(Post) of
                                        {ok, InsertedPost} ->
                                            db_serv:insert_post(Post),
                                            PayloadJsonTerm =
                                                post_to_json_term(InsertedPost),
                                            rest_util:response(
                                              Socket, Request,
                                              {ok, {format, PayloadJsonTerm}});
                                        {error, invalid_post} ->
                                            rest_util:response(
                                              Socket, Request, {error, badarg})
                                    end;
                                {error, invalid} ->
                                    rest_util:response(
                                      Socket, Request, {error, badarg})
                            end;
                        {error, not_found} ->
                            rest_util:response(Socket, Request,
                                               {error, unauthorized})
                    end
            end;
        ["delete_post"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                PostId when is_binary(PostId) ->
                    {ok, #{<<"sessionId">> := SessionId}} =
                        get_bespoke_cookie(Request),
                    case db_user_serv:get_user_from_session_id(
                           base64:decode(SessionId)) of
                        {ok, _User} ->
                            case db_serv:delete_post(PostId) of
                                ok ->
                                    rest_util:response(Socket, Request, ok_204);
                                {error, not_found} ->
                                    rest_util:response(Socket, Request,
                                                       {error, not_found})
                            end;
                        {error, not_found} ->
                            rest_util:response(Socket, Request,
                                               {error, unauthorized})
                    end;
                _ ->
                    rest_util:response(Socket, Request, {error, badarg})
            end;
        ["toggle_like"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                PostId when is_binary(PostId) ->
                    {ok, #{<<"sessionId">> := SessionId}} =
                        get_bespoke_cookie(Request),
                    case db_user_serv:get_user_from_session_id(
                           base64:decode(SessionId)) of
                        {ok, User} ->
                            {ok, Likers} =
                                db_serv:toggle_like(PostId, User#user.id),
                            PayloadJsonTerm =
                                #{<<"liked">> =>
                                      lists:member(User#user.id, Likers),
                                  <<"likesCount">> => length(Likers)},
                            rest_util:response(
                              Socket, Request, {ok, {format, PayloadJsonTerm}});
                        {error, not_found} ->
                            rest_util:response(
                              Socket, Request, {error, unauthorized})
                    end;
                _ ->
                    rest_util:response(Socket, Request, {error, badarg})
            end;
        ["subscribe_on_changes"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                PostIds ->
                    case lists:all(fun(PostId) when is_binary(PostId) ->
                                           true;
                                      (_) ->
                                           false
                                   end, PostIds) of
                        true ->
                            SubscriptionId =
                                db_serv:subscribe_on_changes(PostIds),
                            UpdatedState =
                                State#state{
                                  subscriptions =
                                      maps:put(Socket,
                                               {Request, SubscriptionId},
                                               State#state.subscriptions)},
                            {ok, UpdatedState};
                        false ->
                            rest_util:response(Socket, Request,
                                               {error, badarg})
                    end
            end;
        ["upload_attachments"] ->
            [PayloadJsonTerm] =
                lists:map(fun(#{filename := Filename,
                                unique_filename := UniqueFilename,
                                content_type := ContentType}) ->
                                  AbsPath = filename:join(
                                              [<<"/tmp">>, UniqueFilename]),
                                  #{<<"filename">> => Filename,
                                    <<"absPath">> => AbsPath,
                                    <<"contentType">> => ContentType}
                          end, Body),
            rest_util:response(Socket, Request,
                               {ok, {format, PayloadJsonTerm}});
        _ ->
	    ?log_error("~p not found", [Tokens]),
	    rest_util:response(Socket, Request, {error, not_found})
    end.

%%
%% Authentication
%%

login(Socket, Request, Username, ClientResponse) ->
    case get_challenge_from_cache(Username) of
        {ok, Challenge} ->
            case db_user_serv:get_user_from_username(Username) of
                {ok, #user{password_salt = PasswordSalt,
                           password_hash = PasswordHash}} ->
                    case webapp_auth:verify_client_response(
                           ClientResponse, Challenge, PasswordHash) of
                        true ->
                            {ok, MacAddress} = get_mac_address(Socket),
                            case db_user_serv:login(
                                   Username, MacAddress, PasswordSalt,
                                   PasswordHash) of
                                {ok, #user{id = UserId,
                                           session_id = SessionId}} ->
                                    PayloadJsonTerm =
                                        #{<<"userId">> => UserId,
                                          <<"username">> => Username,
                                          <<"sessionId">> =>
                                              base64:encode(SessionId)},
                                    rest_util:response(
                                      Socket, Request,
                                      {ok, {format, PayloadJsonTerm}});
                                {error, failure} ->
                                    rest_util:response(Socket, Request,
                                                       {error, forbidden})
                            end;
                        false ->
                            rest_util:response(Socket, Request,
                                               {error, forbidden})
                    end;
                {error, not_found} ->
                    rest_util:response(Socket, Request, {error, forbidden})
            end;
        {error, not_found} ->
            rest_util:response(Socket, Request, {error, forbidden})
    end.

switch_user(Socket, Request, Username,
            _PasswordSalt = not_set, _PasswordHash = not_set,
            _ClientResponse = not_set) ->
    {ok, MacAddress} = get_mac_address(Socket),
    case db_user_serv:switch_user(Username, MacAddress) of
        {ok, #user{id = UserId, session_id = SessionId}} ->
            PayloadJsonTerm =
                #{<<"userId">> => UserId,
                  <<"username">> => Username,
                  <<"sessionId">> => base64:encode(SessionId)},
            rest_util:response(Socket, Request,
                               {ok, {format, PayloadJsonTerm}});
        {error, failure} ->
            rest_util:response(Socket, Request, {error, forbidden})
    end;
switch_user(Socket, Request, Username, PasswordSalt, PasswordHash, ClientResponse) ->
    case get_challenge_from_cache(Username) of
        {ok, Challenge} ->
            case db_user_serv:get_user_from_username(Username) of
                {ok, #user{password_hash = not_set}} ->
                    {ok, MacAddress} = get_mac_address(Socket),
                    switch_user_now(
                      Socket, Request, Username, MacAddress, PasswordSalt,
                      PasswordHash);
                {ok, #user{password_salt = PasswordSalt,
                           password_hash = PasswordHash}} ->
                    case webapp_auth:verify_client_response(
                           ClientResponse, Challenge, PasswordHash) of
                        true ->
                            {ok, MacAddress} = get_mac_address(Socket),
                            switch_user_now(
                              Socket, Request, Username, MacAddress,
                              PasswordSalt, PasswordHash);
                        false ->
                            rest_util:response(Socket, Request,
                                               {error, forbidden})
                    end;
                {ok, _} ->
                    rest_util:response(Socket, Request, {error, forbidden});
                {error, not_found} ->
                    {ok, MacAddress} = get_mac_address(Socket),
                    switch_user_now(
                      Socket, Request, Username, MacAddress, PasswordSalt,
                      PasswordHash)
            end;
        {error, not_found} ->
            rest_util:response(Socket, Request, {error, forbidden})
    end.

switch_user_now(Socket, Request, Username, MacAddress, PasswordSalt,
                PasswordHash) ->
    #user{id = UserId, session_id = SessionId} =
        db_user_serv:switch_user(Username, MacAddress, PasswordSalt,
                                 PasswordHash),
    PayloadJsonTerm =
        #{<<"userId">> => UserId,
          <<"username">> => Username,
          <<"sessionId">> => base64:encode(SessionId)},
    rest_util:response(Socket, Request, {ok, {format, PayloadJsonTerm}}).

change_password(Socket, Request, PasswordSalt, PasswordHash) ->
    {ok, MacAddress} = get_mac_address(Socket),
    {ok, #{<<"sessionId">> := SessionId}} = get_bespoke_cookie(Request),
    case db_user_serv:get_user_from_session_id(base64:decode(SessionId)) of
        {ok, #user{name = Username}} ->
            case db_user_serv:change_password(
                   Username, MacAddress, PasswordSalt, PasswordHash) of
                ok ->
                    rest_util:response(Socket, Request, ok_204);
                {error, failure} ->
                    rest_util:response(Socket, Request, {error, forbidden})
            end;
        {error, not_found} ->
            rest_util:response(Socket, Request, {error, unauthorized})
    end.

%%
%% Portal cache
%%

redirect_or_ack(Socket, Request, Page) ->
    {ok, {IpAddress, _Port}} = rester_socket:peername(Socket),
    case ets:lookup(?PORTAL_CACHE, IpAddress) of
        [] ->
            ?log_info("Captive portal redirect"),
            rester_http_server:response_r(
              Socket, Request, 302, "Found", "",
              [{location, "http://bespoke.local/loader.html"}|
               no_cache_headers()]);
        [#portal_cache_entry{timestamp = Timestamp}] ->
            case timestamp() - Timestamp > ?PORTAL_TIMEOUT of
                true ->
                    ?log_info("Captive portal redirect (timeout)"),
                    rester_http_server:response_r(
                      Socket, Request, 302, "Found", "",
                      [{location, "http://bespoke.local/loader.html"}|
                       no_cache_headers()]);
                false ->
                    case Page of
                        "hotspot-detect.html" ->
                            ?log_info("Returning 200 OK (Apple mode)\n"),
                            rester_http_server:response_r(
                              Socket, Request, 200, "OK",
                              "<HTML><HEAD></HEAD><BODY>Success</BODY></HTML>",
                              [{content_type, "text/html"}|no_cache_headers()]);
                        "canonical.html" ->
                            ?log_info("Returning 200 OK (/canonical.html)\n"),
                            rester_http_server:response_r(
                              Socket, Request, 200, "OK",
                              "<HTML><HEAD></HEAD><BODY>Success</BODY></HTML>",
                              [{content_type, "text/html"},
                               no_cache_headers()]);
                        "success.txt" ->
                            ?log_info("Returning 200 OK (/success.txt)\n"),
                            rester_http_server:response_r(
                              Socket, Request, 200, "OK", "success",
                              [{content_type, "text/plain"},
                               no_cache_headers()]);
                        _ ->
                            ?log_info("Returning 204 No Content (generic)\n"),
                            rester_http_server:response_r(
                              Socket, Request, 204, "OK", "",
                              no_cache_headers())
                    end
            end
    end.

delete_all_stale_timestamps() ->
    ?log_info("Purging the captive portal"),
    Threshold = timestamp() - ?PORTAL_TIMEOUT,
    StalePortalCacheEntries =
        ets:foldr(fun(#portal_cache_entry{
                         timestamp = Timestamp} = PortalCacheEntry, Acc)
                        when Timestamp < Threshold ->
                          [PortalCacheEntry|Acc];
                     (_, Acc) ->
                          Acc
                  end, [], ?PORTAL_CACHE),
    %% Clear the MAC addresses
    StaleMacAddresses =
        lists:map(fun(#portal_cache_entry{mac_address = MacAddress}) ->
                          MacAddress
                  end, StalePortalCacheEntries),
    ok = webapp_dnsmasq:clear_mac_addresses(StaleMacAddresses),
    %% Clear the portal cache
    lists:foreach(fun(#portal_cache_entry{ip_address = IpAddress}) ->
                          ?log_info("Purging ~p", [IpAddress]),
                          true = ets:delete(?PORTAL_CACHE, IpAddress)
                  end, StalePortalCacheEntries).

update_portal_cache_entry(Socket) ->
    {ok, {IpAddress, _Port}} = rester_socket:peername(Socket),
    case ets:lookup(?PORTAL_CACHE, IpAddress) of
        [] ->
            true;
        [PortalCacheEntry] ->
            %% Update the timestamp
            true = ets:insert(?PORTAL_CACHE,
                              PortalCacheEntry#portal_cache_entry{
                                timestamp = timestamp()})
    end.

%%
%% Challenge cache
%%

add_challenge_to_cache(UserId, Challenge) ->
    ets:insert(?CHALLENGE_CACHE,
               #challenge_cache_entry{username = UserId,
                                      challenge = Challenge,
                                      timestamp = timestamp()}).

get_challenge_from_cache(Username) ->
    true = purge_challenge_cache(),
    case ets:lookup(?CHALLENGE_CACHE, Username) of
        [] ->
            {error, not_found};
        [#challenge_cache_entry{challenge = Challenge}] ->
            {ok, Challenge}
    end.

purge_challenge_cache() ->
    Threshold = timestamp() - ?CHALLENGE_TIMEOUT,
    ets:foldl(
      fun(#challenge_cache_entry{username = Username,
                                 timestamp = Timestamp}, _Acc)
            when Timestamp < Threshold ->
              ets:delete(?CHALLENGE_CACHE, Username);
         (_, Acc) ->
              Acc
      end, true, ?CHALLENGE_CACHE).

%%
%% Marshalling
%%

post_to_json_term(#post{id = Id,
                        title = Title,
                        parent_post_id = ParentPostId,
                        top_post_id = TopPostId,
                        body = Body,
                        author = Author,
                        created = Created,
                        reply_count = ReplyCount,
                        replies = Replies,
                        likers = Likers}) ->
    JsonTerm = #{<<"id">> => Id,
                 <<"body">> => Body,
                 <<"author">> => Author,
                 <<"created">> => Created,
                 <<"replyCount">> => ReplyCount,
                 <<"replies">> => Replies,
                 <<"likers">> => Likers},
    add_optional_members([{<<"title">>, Title},
                          {<<"parentPostId">>, ParentPostId},
                          {<<"topPostId">>, TopPostId}], JsonTerm).

add_optional_members([], JsonTerm) ->
    JsonTerm;
add_optional_members([{_Key, not_set}|Rest], JsonTerm) ->
    add_optional_members(Rest, JsonTerm);
add_optional_members([{Key, Value}|Rest], JsonTerm) ->
    add_optional_members(Rest, maps:put(Key, Value, JsonTerm)).

json_term_to_bootstrap(#{<<"ssid">> := SSID}) when is_binary(SSID) ->
    {ok, SSID};
json_term_to_bootstrap(_) ->
    {error, invalid}.

json_term_to_login(#{<<"username">> := Username,
                     <<"clientResponse">> := ClientResponse} = JsonTerm)
  when is_binary(Username) andalso is_binary(ClientResponse) ->
    case no_more_keys([<<"username">>, <<"clientResponse">>], JsonTerm) of
        true ->
            {ok, Username, base64:decode(ClientResponse)};
        false ->
            {error, invalid}
    end;
json_term_to_login(_) ->
    {error, invalid}.

json_term_to_switch_user(#{<<"username">> := Username,
                           <<"passwordSalt">> := PasswordSalt,
                           <<"passwordHash">> := PasswordHash,
                           <<"clientResponse">> := ClientResponse} = JsonTerm)
  when is_binary(Username) andalso
       (PasswordSalt == null orelse is_binary(PasswordSalt)) andalso
       (PasswordHash == null orelse is_binary(PasswordHash)) andalso
       (ClientResponse == null orelse is_binary(ClientResponse)) ->
    case no_more_keys([<<"username">>,
                       <<"passwordSalt">>,
                       <<"passwordHash">>,
                       <<"clientResponse">>], JsonTerm) of
        true ->
            {ok, Username, base64decode(PasswordSalt),
             base64decode(PasswordHash), base64decode(ClientResponse)};
        false ->
            {error, invalid}
    end;
json_term_to_switch_user(_) ->
    {error, invalid}.

base64decode(null) ->
    not_set;
base64decode(Value) ->
    base64:decode(Value).

json_term_to_change_password(#{<<"passwordSalt">> := PasswordSalt,
                               <<"passwordHash">> := PasswordHash} = JsonTerm)
  when is_binary(PasswordHash) andalso is_binary(PasswordSalt) ->
    case no_more_keys([<<"passwordSalt">>, <<"passwordHash">>], JsonTerm) of
        true ->
            {ok, base64:decode(PasswordSalt), base64:decode(PasswordHash)};
        false ->
            {error, invalid}
    end;
json_term_to_change_password(_) ->
    {error, invalid}.

%% Top post
json_term_to_post(#{<<"title">> := Title,
                    <<"body">> := Body} = PostJsonTerm, Username)
  when is_binary(Title) andalso is_binary(Body) ->
    case no_more_keys([<<"title">>, <<"body">>],
                      PostJsonTerm) of
        true ->
            {ok, #post{title = Title, body = Body, author = Username}};
        false ->
            {error, invalid}
    end;
%% Reply post
json_term_to_post(#{<<"parentPostId">> := ParentPostId,
                    <<"topPostId">> := TopPostId,
                    <<"body">> := Body} = JsonTerm, Username)
  when is_binary(ParentPostId) andalso
       is_binary(TopPostId) andalso
       is_binary(Body) ->
    case no_more_keys([<<"parentPostId">>,
                       <<"topPostId">>,
                       <<"body">>], JsonTerm) of
        true ->
            {ok, #post{parent_post_id = ParentPostId,
                       top_post_id = TopPostId,
                       body = Body,
                       author = Username}};
        false ->
            {error, invalid}
    end;
json_term_to_post(_, _) ->
    {error, invalid}.

%%
%% Utilities
%%

timestamp() ->
    os:system_time(second).

get_mac_address(Socket) ->
    case rester_socket:peername(Socket) of
        {ok, {{127, 0, 0, 1}, _Port}} ->
            {ok, <<"00:00:00:00:00:00">>};
        {ok, {IpAddress, _Port}} ->
            get_mac_address_for_ip_address(IpAddress)
    end.

get_mac_address_for_ip_address(IpAddress) ->
    Command = "ip neigh show | awk '/" ++
        inet:ntoa(IpAddress) ++ "/ {print $5}'",
    case string:trim(os:cmd(Command)) of
        "" ->
            {error, not_found};
        MacAddress ->
            {ok, ?l2b(MacAddress)}
    end.

get_bespoke_cookie(Request) ->
    get_cookie("bespoke", (Request#http_request.headers)#http_chdr.cookie).

get_cookie(_Name, []) ->
    {errnor, not_found};
get_cookie(Name, [Cookie|Rest]) ->
    case string:tokens(Cookie, "=") of
        [Name, Value] ->
            {ok, json:decode(uri_string:percent_decode(?l2b(Value)))};
        _ ->
            get_cookie(Name, Rest)
    end.

no_cache_headers() ->
    [{"Cache-Control", "no-cache, no-store, must-revalidate"},
     {"Pragma", "no-cache"},
     {"Expires", 0}].

parse_body(Socket, Request, Body) ->
    case rest_util:parse_body(Request, Body) of
        {error, _Reason} ->
            {return, rest_util:response(
                       Socket, Request,
                       {error, bad_request, "Invalid JSON syntax"})};
        JsonTerm ->
            JsonTerm
    end.

change_ssid(SSID) ->
    ScriptPath =
        filename:absname(
          filename:join([code:lib_dir(main), "bin", "change-ssid"])),
    Command = ["sudo bash ", ScriptPath, " ", ?b2l(SSID), " 2>&1; echo $?"],
    ?log_info("Calling: ~s\n", [Command]),
    case string:trim(os:cmd(Command)) of
        "0" ->
            ok;
        UnexpectedOutput ->
            ?log_error("Unexpected output from dnsmasq-tool: ~s",
                       [UnexpectedOutput]),
            {error, UnexpectedOutput}
    end.

no_more_keys(RequiredKeys, Map) ->
    lists:sort(maps:keys(Map)) =:= lists:sort(RequiredKeys).
