-module(webapp_client).
-compile(export_all).

start() ->
    init_httpc(),
    %% Auto login
    {ok,#{<<"noPassword">> := _NoPassword,
          <<"sessionId">> := SessionId,
          <<"userId">> := _UserId,
          <<"username">> := _Username}} =
        http_get("http://localhost/auto_login"),
    %% Fetch all top posts
    {ok, [#{<<"id">> := PostId}|_]} =
        http_get("http://localhost/list_top_posts"),
    %% Fetch specific post(s)
    {ok,[#{<<"id">> := PostId}]} =
        http_post("http://localhost/lookup_posts", [PostId]),
    %% Fetch specific post(s) recursively (include all nested replies)
    {ok,[#{<<"id">> := PostId}|_]} =
        http_post("http://localhost/lookup_recursive_posts", [PostId]),
    %% Switch user
    {ok, #{<<"sessionId">> := NewSessionId,
           <<"userId">> := _NewUserId,
           <<"username">> := <<"foo">>}} =
        http_post("http://localhost/switch_user",
                  #{<<"username">> => <<"foo">>,
                    <<"passwordSalt">> => null,
                    <<"passwordHash">> => null,
                    <<"clientResponse">> => null}),
    %% Insert a top post
    Headers = [{"Cookie", bespoke_cookie(NewSessionId)}],
    {ok, #{<<"id">> := TopPostId}} =
        http_post("http://localhost/insert_post",
                  #{<<"title">> => <<"A new title for a top post">>,
                    <<"body">> => <<"A body">>,
                    <<"attachments">> => []},
                  Headers),
    %% Insert a reply post to the top post
    {ok, #{<<"id">> := _ReplyPostId}} =
        http_post("http://localhost/insert_post",
                  #{<<"parentPostId">> => TopPostId,
                    %% The top post is the parent post in this case
                    <<"topPostId">> => TopPostId,
                    <<"body">> => <<"A reply body">>,
                    <<"attachments">> => []},
                  Headers).

%%
%% Utilities
%%

init_httpc() ->
    application:start(inets),
    inets:start(httpc, [{profile, poop}]).

http_get(Url) ->
    http_get(Url, []).

http_get(Url, Headers) ->
    case httpc:request(get, {Url, [{"connection", "close"}|Headers]}, [], []) of
        {ok, {{"HTTP/1.1", 200, "OK"}, ResponseHeaders, Body}} ->
            case lists:keyfind("content-type", 1, ResponseHeaders) of
                {_, "application/json"} ->
                    {ok, json:decode(iolist_to_binary(Body))};
                _ ->
                    {ok, Body}
            end;
        {ok, {{"HTTP/1.1", StatusCode, ReasonPhrase}}, ResponseHeaders, Body} ->
            {unexpected, StatusCode, ReasonPhrase, ResponseHeaders, Body};
        {error, Reason} ->
            {error, Reason}
    end.

http_post(Url, Data) ->
    http_post(Url, Data, []).

http_post(Url, Data, Headers) ->
    case httpc:request(
           post, {Url, [{"connection", "close"}|Headers],
                  "application/json", json:encode(Data)}, [], []) of
        {ok, {{"HTTP/1.1", 200, "OK"}, ResponseHeaders, Body}} ->
            case lists:keyfind("content-type", 1, ResponseHeaders) of
                {_, "application/json"} ->
                    {ok, json:decode(iolist_to_binary(Body))};
                _ ->
                    {ok, Body}
            end;
        {ok, {{"HTTP/1.1", StatusCode, ReasonPhrase}}, ResponseHeaders, Body} ->
            {unexpected, StatusCode, ReasonPhrase, ResponseHeaders, Body};
        {error, Reason} ->
            {error, Reason}
    end.

bespoke_cookie(SessionId) ->
    lists:flatten(
      io_lib:format(
        "bespoke=~s",
        [http_uri:encode(json:encode(#{<<"sessionId">> => SessionId}))])).
