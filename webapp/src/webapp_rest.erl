-module(webapp_rest).
-export([start_link/0]).
%% rester_http_server callbacks
-export([init/2, info/3, close/2, error/3, http_request/4]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("rester/include/rester.hrl").
-include_lib("rester/include/rester_http.hrl").
-include_lib("rester/include/rester_socket.hrl").
-include_lib("db/include/db.hrl").

-define(CAPTIVE_PORTAL_CACHE, captive_portal_cache).
-define(LEASETIME, 7200).

-record(state, {
                subscriptions = #{} ::
                  #{rester_socket() =>
                        {Request :: term(), db_serv:subscription_id()}}
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
    ?CAPTIVE_PORTAL_CACHE =
        ets:new(?CAPTIVE_PORTAL_CACHE, [public, named_table]),
    ?log_info("Database REST API has been started"),
    HttpPort = application:get_env(webapp, http_port, 80),
    rester_http_server:start_link(HttpPort, Options),
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
            case ets:lookup(?CAPTIVE_PORTAL_CACHE, MacAddress) of
                [] ->
                    ?log_info("Captive portal ack (not found)\n"),
                    ets:insert(?CAPTIVE_PORTAL_CACHE,
                               {MacAddress, timestamp()}),
                    rester_http_server:response_r(
                      Socket, Request, 204, "OK", "", no_cache_headers());
                [{MacAddress, _Timestamp}] ->
                    ?log_info("Captive portal ack (found)\n"),
                    ets:insert(?CAPTIVE_PORTAL_CACHE,
                               {MacAddress, timestamp()}),
                    rester_http_server:response_r(
                      Socket, Request, 204, "OK", "", no_cache_headers())
            end;
        %% Bespoke API
        ["list_top_posts"] ->
            Posts = db_serv:list_top_posts(),
            PayloadJsonTerm = lists:map(fun(Post) ->
                                                post_to_json_term(Post)
                                        end, Posts),
            rest_util:response(Socket, Request,
                               {ok, {format, PayloadJsonTerm}});
        ["auto_login"] ->
            %% does the file /var/tmp/bespoke.bootstrap exist?
            case filelib:is_regular("/var/tmp/bespoke.bootstrap") of
                true ->
                    rester_http_server:response_r(
                      Socket, Request, 302, "Found", "",
                      [{location, "/bootstrap.html"}|no_cache_headers()]);
                false ->
                    {ok, MacAddress} = get_mac_address(Socket),
                    User = db_user_serv:get_user_from_mac_address(MacAddress),
                    PayloadJsonTerm =
                        #{<<"no-password">> => true,
                          <<"user-id">> => User#user.id,
                          <<"username">> => User#user.name,
                          <<"session-id">> => User#user.session_id},
                    case User#user.pwhash of
                        not_set ->
                            rest_util:response(Socket, Request,
                                               {ok, {format, PayloadJsonTerm}});
                        _ ->
                            UpdatedPayloadJsonTerm =
                                maps:put(<<"no-password">>, false,
                                         PayloadJsonTerm),
                            rest_util:response(
                              Socket, Request,
                              {ok, {format, UpdatedPayloadJsonTerm}})
                    end
            end;
        %% Act as static web server
	Tokens ->
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
                        {ok, _SSID} ->
                            ok = file:delete("/var/tmp/bespoke.bootstrap"),
                            %% FIXME: Do something meaningful with SSID
                            rest_util:response(Socket, Request, ok_204);
                        {error, invalid} ->
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request, "Invalid JSON format"})
                    end
            end;
        ["login"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                JsonTerm ->
                    case json_term_to_login(JsonTerm) of
                        {ok, Username, Password} ->
                            case db_user_serv:login(Username, Password) of
                                {ok, #user{id = UserId,
                                           name = Username,
                                           session_id = SessionId}} ->
                                    PayloadJsonTerm =
                                        #{<<"user-id">> => UserId,
                                          <<"username">> => Username,
                                          <<"session-id">> => SessionId},
                                    rest_util:response(
                                      Socket, Request,
                                      {ok, {format, PayloadJsonTerm}});
                                {error, failure} ->
                                    rest_util:response(Socket, Request,
                                                       {error, no_access})
                            end;
                        {error, invalid} ->
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request, "Invalid JSON format"})
                    end
            end;
        ["switch_user"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                JsonTerm ->
                    case json_term_to_switch_user(JsonTerm) of
                        {ok, Username, Password} ->
                            {ok, MacAddress} = get_mac_address(Socket),
                            case db_user_serv:switch_user(
                                   Username, Password, MacAddress) of
                                {ok, User} ->
                                    PayloadJsonTerm =
                                        #{<<"user-id">> => User#user.id,
                                          <<"username">> => User#user.name,
                                          <<"session-id">> =>
                                              User#user.session_id},
                                    rest_util:response(
                                      Socket, Request,
                                      {ok, {format, PayloadJsonTerm}});
                                {error, failure} ->
                                    rest_util:response(Socket, Request,
                                                       {error, no_access})
                            end;
                        {error, invalid} ->
                            rest_util:response(Socket, Request,
                                               {error, no_access})
                    end
            end;
        ["change_password"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                JsonTerm ->
                    case json_term_to_change_password(JsonTerm) of
                        {ok, Password} ->
                            {ok, MacAddress} = get_mac_address(Socket),
                            {ok, #{<<"sessionId">> := SessionId}} =
                                get_bespoke_cookie(Request),
                            case db_user_serv:get_user_from_session_id(
                                   SessionId) of
                                {ok, #user{name = Username}} ->
                                    case db_user_serv:change_password(
                                           Username, Password, MacAddress) of
                                        ok ->
                                            rest_util:response(
                                              Socket, Request, ok_204);
                                        {error, failure} ->
                                            rest_util:response(
                                              Socket, Request,
                                              {error, no_access})
                                    end;
                                {error, not_found} ->
                                    rest_util:response(Socket, Request,
                                                       {error, no_access})
                            end;
                        {error, invalid} ->
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request, "Invalid JSON format"})
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
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request,
                               "post-ids must be strings"})
                    end;
                _ ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"})
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
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request,
                               "post-ids must be strings"})
                    end;
                _ ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"})
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
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request,
                               "post-ids must be strings"})
                    end;
                _ ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"})
            end;
        ["insert_post"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                JsonTerm ->
                    {ok, #{<<"sessionId">> := SessionId}} =
                        get_bespoke_cookie(Request),
                    case db_user_serv:get_user_from_session_id(SessionId) of
                        {ok, #user{name = Username}} ->
                            case json_term_to_post(JsonTerm, Username) of
                                {ok, Post} ->
                                    {ok, InsertedPost} =
                                        db_serv:insert_post(Post),
                                    PayloadJsonTerm =
                                        post_to_json_term(InsertedPost),
                                    rest_util:response(
                                      Socket, Request,
                                      {ok, {format, PayloadJsonTerm}});
                                {error, invalid} ->
                                    rest_util:response(
                                      Socket, Request,
                                      {error, bad_request, "Invalid post"})
                            end;
                        {error, not_found} ->
                            rest_util:response(Socket, Request,
                                               {error, no_access})
                    end
            end;
        ["delete_post"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                PostId when is_binary(PostId) ->
                    {ok, #{<<"sessionId">> := SessionId}} =
                        get_bespoke_cookie(Request),
                    case db_user_serv:get_user_from_session_id(SessionId) of
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
                                               {error, no_access})
                    end;
                _ ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "post-id must be a string"})
            end;
        ["toggle_like"] ->
            case parse_body(Socket, Request, Body) of
                {return, Result} ->
                    Result;
                PostId when is_binary(PostId) ->
                    {ok, #{<<"sessionId">> := SessionId}} =
                        get_bespoke_cookie(Request),
                    case db_user_serv:get_user_from_session_id(SessionId) of
                        {ok, User} ->
                            {ok, Likers} =
                                db_serv:toggle_like(PostId, User#user.id),
                            PayloadJsonTerm =
                                #{<<"liked">> =>
                                      lists:member(User#user.id, Likers),
                                  <<"likes-count">> => length(Likers)},
                            rest_util:response(
                              Socket, Request, {ok, {format, PayloadJsonTerm}});
                        {error, not_found} ->
                            rest_util:response(Socket, Request,
                                               {error, no_access})
                    end;
                _ ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "post-id must be a string"})
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
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request, "post-ids must be strings"})
                    end
            end;
        _ ->
	    ?log_error("~p not found", [Tokens]),
	    rest_util:response(Socket, Request, {error, not_found})
    end.

%%
%% Captive portal
%%

redirect_or_ack(Socket, Request, Page) ->
    {ok, MacAddress} = get_mac_address(Socket),
    case ets:lookup(?CAPTIVE_PORTAL_CACHE, MacAddress) of
        [] ->
            ?log_info("Captive portal redirect...\n"),
            rester_http_server:response_r(
              Socket, Request, 302, "Found", "",
              [{location, "http://bespoke.local/loader.html"}|
               no_cache_headers()]);
        [{MacAddress, Timestamp}] ->
            %% 2 hours timeout (sync with leasetime in /etc/dhcpcd.conf)
            case timestamp() - Timestamp > ?LEASETIME of
                true ->
                    ?log_info("Captive portal redirect (timeout)\n"),
                    ok = delete_all_stale_timestamps(),
                    rester_http_server:response_r(
                      Socket, Request, 302, "Found", "",
                      [{location, "http://bespoke.local/loader.html"}|
                       no_cache_headers()]);
                false ->
                    case Page of
                        "hotspot-detect.html" ->
                            ?log_info("Returning 200 OK (Apple mode)\n"),
                            true = ets:insert(?CAPTIVE_PORTAL_CACHE,
                                              {MacAddress, timestamp()}),
                            rester_http_server:response_r(
                              Socket, Request, 200, "OK",
                              "<HTML><HEAD></HEAD><BODY>Success</BODY></HTML>",
                              [{content_type, "text/html"}|no_cache_headers()]);
                        "canonical.html" ->
                            ?log_info("Returning 200 OK (/canonical.html)\n"),
                            ets:insert(?CAPTIVE_PORTAL_CACHE,
                                       {MacAddress, timestamp()}),
                            rester_http_server:response_r(
                              Socket, Request, 200, "OK",
                              "<HTML><HEAD></HEAD><BODY>Success</BODY></HTML>",
                              [{content_type, "text/html"},
                               no_cache_headers()]);
                        "success.txt" ->
                            ?log_info("Returning 200 OK (/success.txt)\n"),
                            ets:insert(?CAPTIVE_PORTAL_CACHE,
                                       {MacAddress, timestamp()}),
                            rester_http_server:response_r(
                              Socket, Request, 200, "OK", "success",
                              [{content_type, "text/plain"},
                               no_cache_headers()]);
                        _ ->
                            ?log_info("Returning 204 No Content (generic)\n"),
                            ets:insert(?CAPTIVE_PORTAL_CACHE,
                                       {MacAddress, timestamp()}),
                            rester_http_server:response_r(
                              Socket, Request, 204, "OK", "",
                              no_cache_headers())
                    end
            end
    end.

delete_all_stale_timestamps() ->
    Threshold = timestamp() - ?LEASETIME,
    StaleMacAddresses =
        ets:foldr(fun({MacAddress, Timestamp}, Acc)
                        when Timestamp < Threshold ->
                          [MacAddress|Acc];
                     (_, Acc) ->
                          Acc
                  end, [], ?CAPTIVE_PORTAL_CACHE),
    ok = webapp_dnsmasq:clear_mac_addresses(StaleMacAddresses),
    lists:foreach(fun(MacAddress) ->
                          ets:delete(?CAPTIVE_PORTAL_CACHE, MacAddress)
                  end, StaleMacAddresses).

%%
%% Marshalling
%%

json_term_to_bootstrap(#{<<"ssid">> := SSID}) when is_binary(SSID) ->
    {ok, SSID};
json_term_to_bootstrap(_) ->
    {error, invalid}.

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
                 <<"reply-count">> => ReplyCount,
                 <<"replies">> => Replies,
                 <<"likers">> => Likers},
    add_optional_members([{<<"title">>, Title},
                          {<<"parent-post-id">>, ParentPostId},
                          {<<"top-post-id">>, TopPostId}], JsonTerm).

add_optional_members([], JsonTerm) ->
    JsonTerm;
add_optional_members([{_Key, not_set}|Rest], JsonTerm) ->
    add_optional_members(Rest, JsonTerm);
add_optional_members([{Key, Value}|Rest], JsonTerm) ->
    add_optional_members(Rest, maps:put(Key, Value, JsonTerm)).

%% Top post
json_term_to_post(#{<<"title">> := Title,
                    <<"body">> := Body} = PostJsonTerm, Username) ->
    case no_more_keys([<<"title">>, <<"body">>],
                      PostJsonTerm) of
        true ->
            {ok, #post{title = Title, body = Body, author = Username}};
        false ->
            {error, invalid}
    end;
%% Reply post
json_term_to_post(#{<<"parent-post-id">> := ParentPostId,
                    <<"top-post-id">> := TopPostId,
                    <<"body">> := Body} = JsonTerm, Username) ->
    case no_more_keys([<<"parent-post-id">>,
                       <<"top-post-id">>,
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

no_more_keys(RequiredKeys, Map) ->
    lists:sort(maps:keys(Map)) =:= lists:sort(RequiredKeys).

json_term_to_login(
  #{<<"username">> := Username,
    <<"password">> := Password} = JsonTerm) ->
    case no_more_keys([<<"username">>, <<"password">>], JsonTerm) of
        true ->
            {ok, Username, Password};
        false ->
            {error, invalid}
    end.

json_term_to_switch_user(
  #{<<"username">> := Username,
    <<"password">> := Password} = JsonTerm) ->
    case no_more_keys([<<"username">>, <<"password">>], JsonTerm) of
        true ->
            {ok, Username, Password};
        false ->
            {error, invalid}
    end.

json_term_to_change_password(Password) when is_binary(Password) ->
    {ok, Password};
json_term_to_change_password(_) ->
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
    {error, not_found};
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
                       {error, bad_request, "Invalid JSON format"})};
        JsonTerm ->
            JsonTerm
    end.
