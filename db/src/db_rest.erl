-module(db_rest).
-export([start_link/0]).
-export([request_handler/4]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("rester/include/rester.hrl").
-include_lib("rester/include/rester_http.hrl").
-include("db.hrl").

-define(CAPTIVE_PORTAL_CACHE, captive_portal_cache).
-define(LEASETIME, 7200).

%%
%% Exported: start_link
%%

start_link() ->
    ok = db_dnsmasq:clear_all_mac_addresses(),
    Options =
	[{request_handler, {?MODULE, request_handler, []}},
         %% FIXME!!! Enable SSL or else passwords will be sent in the clear
         %%	 {verify, verify_none},
         %%         {cacerts, []},
         %%	 {certfile, filename:join([code:priv_dir(db), "cert.pem"])},
	 {nodelay, true},
	 {reuseaddr, true}],
    ?log_info("Database REST API has been started"),
    ?CAPTIVE_PORTAL_CACHE =
        ets:new(?CAPTIVE_PORTAL_CACHE, [public, named_table]),
    rester_http_server:start_link(80, Options).

%%
%% Exported: request_handler
%%

request_handler(Socket, Request, Body, Options) ->
    ?log_info("Request = ~s, Headers = ~s, Body = ~p",
              [rester_http:format_request(Request),
               rester_http:format_hdr(Request#http_request.headers),
               Body]),
    try request_handler_(Socket, Request, Body, Options) of
	Result ->
            Result
    catch
	_Class:Reason:StackTrace ->
	    ?log_error("request_handler crashed: ~p\n~p\n",
                       [Reason, StackTrace]),
	    erlang:error(Reason)
    end.

request_handler_(Socket, Request, Body, Options) ->
    case Request#http_request.method of
	'GET' ->
	    http_get(Socket, Request, Body, Options);
	'POST' ->
	    http_post(Socket, Request, Body, Options);
	_ ->
	    rest_util:response(Socket, Request, {error, not_allowed})
    end.

http_get(Socket, Request, Body, Options) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["versions"] ->
	    Object = json:encode([<<"v1">>]),
	    rester_http_server:response_r(Socket, Request, 200, "OK", Object,
                                          [{content_type, "application/json"}]);
	["v1"|Tokens] ->
	    http_get(Socket, Request, Options, Url, Tokens, Body, v1);
	Tokens ->
	    http_get(Socket, Request, Options, Url, Tokens,  Body, v1)
    end.

http_get(Socket, Request, _Options, Url, Tokens, _Body, v1) ->
    Headers = Request#http_request.headers,
    case Tokens of
        %% On Ubuntu Core, the network manager will check for connectivity
        _ when Headers#http_chdr.host == "connectivity-check.ubuntu.com." orelse
               Headers#http_chdr.host == "connectivity-check.ubuntu.com" ->
            io:format("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, ubuntu);
        %% In Firefox, the browser will check for a captive portal
        %% https://support.mozilla.org/en-US/kb/captive-portal
        ["canonical.html" = Page]
          when Headers#http_chdr.host == "detectportal.firefox.com" ->
            io:format("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, Page);
        ["success.txt" = Page]
          when Headers#http_chdr.host == "detectportal.firefox.com" ->
            io:format("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, Page);
        %% Apple devices will check for a captive portal
        ["hotspot-detect.html" = Page] ->
            io:format("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, Page);
        %% Android devices will check for a captive portal
        ["generate_204" = Page] ->
            io:format("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, Page);
        ["gen_204" = Page] ->
            io:format("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            redirect_or_ack(Socket, Request, Page);
        %% Captive portal ack generated by loader.js on load
        ["captive_portal_ack"] ->
            io:format("Request to ~s~s\n",
                      [Headers#http_chdr.host, Url#url.path]),
            {ok, MacAddress} = get_mac_address(Socket),
            ok = db_dnsmasq:set_post_login_mac_address(MacAddress),
            case ets:lookup(?CAPTIVE_PORTAL_CACHE, MacAddress) of
                [] ->
                    io:format("Captive portal ack (not found)\n"),
                    ets:insert(?CAPTIVE_PORTAL_CACHE, {MacAddress, timestamp()}),
                    rester_http_server:response_r(
                      Socket, Request, 204, "OK", "", no_cache_headers());
                [{MacAddress, _Timestamp}] ->
                    io:format("Captive portal ack (found)\n"),
                    ets:insert(?CAPTIVE_PORTAL_CACHE, {MacAddress, timestamp()}),
                    rester_http_server:response_r(
                      Socket, Request, 204, "OK", "", no_cache_headers())
            end;
        %% Bespoke API
        ["list_top_posts"] ->
            Posts = db_serv:list_top_posts(),
            JsonTerm = lists:map(fun(Post) ->
                                         post_to_json_term(Post)
                                 end, Posts),
            rest_util:response(Socket, Request, {ok, {format, JsonTerm}});
        ["login"] ->
            {ok, MacAddress} = get_mac_address(Socket),
            User = db_user_serv:get_user_from_mac_address(MacAddress),
            UserJsonTerm =
                #{<<"no-password">> => true,
                  <<"username">> => User#user.name,
                  <<"session-id">> => User#user.session_id},
            case User#user.pwhash of
                not_set ->
                    rest_util:response(Socket, Request,
                                       {ok, {format, UserJsonTerm}});
                _ ->
                    UpdatedJsonTerm =
                        maps:put(<<"no-password">>, false, UserJsonTerm),
                    rest_util:response(Socket, Request,
                                       {ok, {format, UpdatedJsonTerm}})
            end;
        ["get_username"] ->
            {ok, MacAddress} = get_mac_address(Socket),
            User = db_user_serv:get_user_from_mac_address(MacAddress),
            rest_util:response(Socket, Request, {ok, {format, User#user.name}});
        %% Act as static web server
	Tokens when Headers#http_chdr.host == "localhost" orelse
                    Headers#http_chdr.host == "bespoke.local" orelse
                    Headers#http_chdr.host == "bespoke" ->
            UriPath =
                case Tokens of
                    [] ->
                        "/top_posts.html";
                    _ ->
                        Url#url.path
                end,
            case UriPath of
                "/top_posts.html" ->
                    io:format("Returning /top_posts.html\n");
                _ ->
                    silent
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
            end;
        _ ->
            io:format("Unexpected request: ~p\n", [{Request, Url, Tokens}]),
            rest_util:response(Socket, Request, {error, not_found})
    end.

no_cache_headers() ->
    [{"Cache-Control", "no-cache, no-store, must-revalidate"},
     {"Pragma", "no-cache"},
     {"Expires", 0}].

redirect_or_ack(Socket, Request, Page) ->
    {ok, MacAddress} = get_mac_address(Socket),
    case ets:lookup(?CAPTIVE_PORTAL_CACHE, MacAddress) of
        [] ->
            io:format("Captive portal redirect...\n"),
            rester_http_server:response_r(
              Socket, Request, 302, "Found", "",
              [{location, "http://bespoke.local/login.html"}|
               no_cache_headers()]);
        [{MacAddress, Timestamp}] ->
            %% 2 hours timeout (sync with leasetime in /etc/dhcpcd.conf)
            case timestamp() - Timestamp > ?LEASETIME of
                true ->
                    io:format("Captive portal redirect (timeout)\n"),
                    ok = delete_all_stale_timestamps(),
                    rester_http_server:response_r(
                      Socket, Request, 302, "Found", "",
                      [{location, "http://bespoke.local/login.html"}|
                       no_cache_headers()]);
                false ->
                    case Page of
                        "hotspot-detect.html" ->
                            io:format("Returning 200 OK (Apple mode)\n"),
                            true = ets:insert(?CAPTIVE_PORTAL_CACHE,
                                              {MacAddress, timestamp()}),
                            rester_http_server:response_r(
                              Socket, Request, 200, "OK",
                              "<HTML><HEAD></HEAD><BODY>Success</BODY></HTML>",
                              [{content_type, "text/html"}|no_cache_headers()]);
                        "canonical.html" ->
                            io:format("Returning 200 OK (/canonical.html)\n"),
                            ets:insert(?CAPTIVE_PORTAL_CACHE,
                                       {MacAddress, timestamp()}),
                            rester_http_server:response_r(
                              Socket, Request, 200, "OK",
                              "<HTML><HEAD></HEAD><BODY>Success</BODY></HTML>",
                              [{content_type, "text/html"},
                               no_cache_headers()]);
                        "success.txt" ->
                            io:format("Returning 200 OK (/success.txt)\n"),
                            ets:insert(?CAPTIVE_PORTAL_CACHE,
                                       {MacAddress, timestamp()}),
                            rester_http_server:response_r(
                              Socket, Request, 200, "OK", "success",
                              [{content_type, "text/plain"},
                               no_cache_headers()]);
                        _ ->
                            io:format("Returning 204 No Content (generic)\n"),
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
    ok = db_dnsmasq:clear_mac_addresses(StaleMacAddresses),
    lists:foreach(fun(MacAddress) ->
                          ets:delete(?CAPTIVE_PORTAL_CACHE, MacAddress)
                  end, StaleMacAddresses).

http_post(Socket, Request, Body, Options) ->
    Url = Request#http_request.uri,
    case string:tokens(Url#url.path, "/") of
	["v1" | Tokens] ->
	    http_post(Socket, Request, Options, Url, Tokens, Body, v1);
	Tokens ->
	    http_post(Socket, Request, Options, Url, Tokens, Body, v1)
    end.

http_post(Socket, Request, _Options, _Url, Tokens, Body, v1) ->
    case Tokens of
        ["authenticate"] ->
            case rest_util:parse_body(Request, Body) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
                AuthenticateJsonTerm ->
                    case json_term_to_authenticate(AuthenticateJsonTerm) of
                        {ok, Username, Password} ->
                            case db_user_serv:authenticate(Username, Password) of
                                {ok, User} ->
                                    rest_util:response(
                                      Socket, Request,
                                      {ok, {format, User#user.session_id}});
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
        ["lookup_posts"] ->
            case rest_util:parse_body(Request, Body) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
                PostIds when is_list(PostIds) ->
                    case lists:all(fun(PostId) when is_binary(PostId) ->
                                           true;
                                      (_) ->
                                           false
                                   end, PostIds) of
                        true ->
                            Posts = db_serv:lookup_posts(PostIds),
                            JsonTerm =
                                lists:map(fun(Post) ->
                                                  post_to_json_term(Post)
                                          end, Posts),
                            rest_util:response(
                              Socket, Request, {ok, {format, JsonTerm}});
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
            case rest_util:parse_body(Request, Body) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
                PostIds when is_list(PostIds) ->
                    case lists:all(fun(PostId) when is_binary(PostId) ->
                                           true;
                                      (_) ->
                                           false
                                   end, PostIds) of
                        true ->
                            Posts =
                                db_serv:lookup_posts(PostIds, recursive),
                            JsonTerm =
                                lists:map(fun(Post) ->
                                                  post_to_json_term(Post)
                                          end, Posts),
                            rest_util:response(
                              Socket, Request, {ok, {format, JsonTerm}});
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
            case rest_util:parse_body(Request, Body) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
                PostJsonTerm ->
                    {ok, #{<<"sessionId">> := SessionId}} =
                        get_bespoke_cookie(Request),
                    case db_user_serv:get_user_from_session_id(SessionId) of
                        {ok, #user{name = Username}} ->
                            case json_term_to_post(PostJsonTerm, Username) of
                                {ok, Post} ->
                                    {ok, InsertedPost} =
                                        db_serv:insert_post(Post),
                                    InsertedPostJsonTerm =
                                        post_to_json_term(InsertedPost),
                                    rest_util:response(
                                      Socket, Request,
                                      {ok, {format, InsertedPostJsonTerm}});
                                {error, invalid} ->
                                    rest_util:response(Socket, Request,
                                                       {error, no_access})
                            end;
                        {error, not_found} ->
                            rest_util:response(
                              Socket, Request,
                              {error, bad_request, "Invalid post"})
                    end
            end;
        ["delete_post"] ->
            case rest_util:parse_body(Request, Body) of
                {error, _Reason} ->
                    rest_util:response(
                      Socket, Request,
                      {error, bad_request, "Invalid JSON format"});
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
	_ ->
	    ?log_error("~p not found", [Tokens]),
	    rest_util:response(Socket, Request, {error, not_found})
    end.

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
                        replies = Replies}) ->
    JsonTerm = #{<<"id">> => Id,
                 <<"body">> => Body,
                 <<"author">> => Author,
                 <<"created">> => Created,
                 <<"reply-count">> => ReplyCount,
                 <<"replies">> => Replies},
    add_optional_members([{<<"title">>, Title},
                          {<<"parent-post-id">>, ParentPostId},
                          {<<"top-post-id">>, TopPostId}], JsonTerm).

add_optional_members([], JsonTerm) ->
    JsonTerm;
add_optional_members([{_Key, not_set}|Rest], JsonTerm) ->
    add_optional_members(Rest, JsonTerm);
add_optional_members([{Key, Value}|Rest], JsonTerm) ->
    add_optional_members(Rest, maps:put(Key, Value, JsonTerm)).

json_term_to_post(#{<<"title">> := Title,
                    <<"body">> := Body} = PostJsonTerm, Username) ->
    case no_more_keys([<<"title">>, <<"body">>],
                      PostJsonTerm) of
        true ->
            {ok, #post{title = Title, body = Body, author = Username}};
        false ->
            {error, invalid}
    end;
json_term_to_post(#{<<"parent-post-id">> := ParentPostId,
                    <<"top-post-id">> := TopPostId,
                    <<"body">> := Body} = PostJsonTerm, Username) ->
    case no_more_keys([<<"parent-post-id">>,
                       <<"top-post-id">>,
                       <<"body">>], PostJsonTerm) of
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

json_term_to_authenticate(
  #{<<"username">> := Username,
    <<"password">> := Password} = AuthenticateJsonTerm) ->
    case no_more_keys([<<"username">>, <<"password">>], AuthenticateJsonTerm) of
        true ->
            {ok, Username, Password};
        false ->
            {error, invalid}
    end.

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
    io:format("*********COMMAND ~p\n",[Command]),
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
            {ok, json:decode(?l2b(uri_string:percent_decode(Value)))};
        _ ->
            get_cookie(Name, Rest)
    end.
