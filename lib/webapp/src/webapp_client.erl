-module(webapp_client).
-compile(export_all).

start() ->
    init_httpc(),
    %% Auto login
    {ok,#{<<"noPassword">> := _NoPassword,
          <<"sessionId">> := _SessionId,
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
    {ok,[#{<<"id">> := _PostId2}|_]} =
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
                    <<"body">> => <<"A body">>},
                  Headers),
    %% Insert a reply post to the top post (including one attachment)
    FilePath =
        filename:join(
          [code:priv_dir(webapp), "docroot/images/animated-background.gif"]),
    UploadedFile =
        http_multipart_post("http://localhost/upload_attachments", FilePath),
    {ok, #{<<"id">> := _ReplyPostId}} =
        http_post("http://localhost/insert_post",
                  #{<<"parentPostId">> => TopPostId,
                    %% The top post is the parent post in this case
                    <<"topPostId">> => TopPostId,
                    %% One hour back in time
                    <<"created">> => os:system_time(second) - 3600,
                    <<"body">> => <<"A reply body">>,
                    %% One attachment
                    <<"attachments">> => [make_attachment(UploadedFile)]},
                  Headers).

make_attachment(#{<<"absPath">> := AbsPath,
                  <<"contentType">> := ContentType}) ->
    <<"/tmp/", TmpFilename/binary>> = AbsPath,
    #{<<"filename">> => TmpFilename,
      <<"contentType">> => ContentType}.

%%
%% Utilities
%%

init_httpc() ->
    application:start(inets),
    inets:start(httpc, [{profile, poop}]).

http_get(Url) ->
    http_get(Url, []).

http_get(Url, Headers) ->
    handle_response(
      httpc:request(get, {Url, [{"connection", "close"}|Headers]}, [], [])).

http_post(Url, Data) ->
    http_post(Url, Data, []).

http_post(Url, Data, Headers) ->
    handle_response(
      httpc:request(post, {Url, [{"connection", "close"}|Headers],
                           "application/json", json:encode(Data)}, [], [])).
handle_response({ok, {{"HTTP/1.1", 200, "OK"},
                      ResponseHeaders, Body}}) ->
    case lists:keyfind("content-type", 1, ResponseHeaders) of
        {_, "application/json"} ->
            {ok, json:decode(iolist_to_binary(Body))};
        _ ->
            {ok, Body}
    end;
handle_response({ok, {{"HTTP/1.1", StatusCode, ReasonPhrase}},
                 ResponseHeaders, Body}) ->
    {unexpected, StatusCode, ReasonPhrase, ResponseHeaders, Body};
handle_response({error, Reason}) ->
    {error, Reason}.

http_multipart_post(Url, FilePath) ->
    %% httpc does not support multipart/form-data
    Result = os:cmd("curl -s -X POST -F 'filename=@" ++ FilePath ++ "' " ++ Url),
    json:decode(list_to_binary(Result)).

bespoke_cookie(SessionId) ->
    lists:flatten(
      io_lib:format(
        "bespoke=~s",
        [http_uri:encode(json:encode(#{<<"sessionId">> => SessionId}))])).
