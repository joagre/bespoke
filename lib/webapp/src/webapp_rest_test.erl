% -*- fill-column: 100; -*-

-module(webapp_rest_test).
-export([misc/0, messaging/0]).

%%
%% Exported: misc
%%

%% IMPORTANT: Run webapp_client:run_once() before running this test

misc() ->
    _ = webapp_client:init_httpc(),
    %% Auto login
    {ok, #{<<"noPassword">> := _NoPassword,
           <<"sessionId">> := SessionId,
           <<"userId">> := _UserId,
           <<"username">> := _Username}} =
        webapp_client:http_get("http://localhost/api/auto_login"),
    %% Fetch all top posts
    Headers = [{"Cookie", webapp_client:bespoke_cookie(SessionId)}],
    {ok, [#{<<"id">> := PostId}|_]} =
        webapp_client:http_get("http://localhost/api/list_top_posts", Headers),
    %% Fetch specific post(s)
    {ok, [#{<<"id">> := PostId}]} =
        webapp_client:http_post("http://localhost/api/lookup_posts", [PostId], Headers),
    %% Fetch specific post(s) recursively (include all nested replies)
    {ok, [#{<<"id">> := _PostId2}|_]} =
        webapp_client:http_post("http://localhost/api/lookup_recursive_posts", [PostId],
                                Headers),
    %% Switch user
    {ok, #{<<"sessionId">> := NewSessionId,
           <<"userId">> := _NewUserId,
           <<"username">> := <<"foo">>}} =
        webapp_client:http_post("http://localhost/api/switch_user",
                                #{<<"username">> => <<"foo">>,
                                  <<"passwordSalt">> => null,
                                  <<"passwordHash">> => null,
                                  <<"clientResponse">> => null},
                                Headers),
    %% Insert a top post
    NewHeaders = [{"Cookie", webapp_client:bespoke_cookie(NewSessionId)}],
    {ok, #{<<"id">> := TopPostId}} =
        webapp_client:http_post("http://localhost/api/insert_post",
                                #{<<"title">> => <<"A new title for a top post">>,
                                  <<"body">> => <<"A body">>},
                                NewHeaders),
    %% Insert a reply post to the top post (including one attachment)
    FilePath =
        filename:join(
          [code:priv_dir(webapp), "docroot/images/animated-background.gif"]),
    UploadedFile =
        webapp_client:http_multipart_post("http://localhost/api/upload_file", FilePath),
    {ok, #{<<"id">> := _ReplyPostId}} =
        webapp_client:http_post("http://localhost/api/insert_post",
                                #{<<"parentPostId">> => TopPostId,
                                  %% The top post is the parent post in this case
                                  <<"topPostId">> => TopPostId,
                                  %% One hour back in time
                                  <<"created">> => os:system_time(second) - 3600,
                                  <<"body">> => <<"A reply body">>,
                                  %% One attachment
                                  <<"attachments">> => [make_attachment(UploadedFile)]},
                                NewHeaders).

make_attachment(#{<<"absPath">> := AbsPath, <<"contentType">> := ContentType}) ->
    <<"/tmp/", TmpFilename/binary>> = AbsPath,
    #{<<"filename">> => TmpFilename, <<"contentType">> => ContentType}.

%%
%% Exported: messaging
%%

%% IMPORTANT: Perform a "make reset" before running this test

messaging() ->
    _ = webapp_client:init_httpc(),
    %% Auto login
    {ok, #{<<"noPassword">> := _NoPassword,
           <<"sessionId">> := SessionId,
           <<"userId">> := _UserId,
           <<"username">> := _Username}} =
        webapp_client:http_get("http://localhost/api/auto_login"),
    %% Create users
    [#{<<"userId">> := FooUserId, <<"sessionId">> := FooSessionId},
     #{<<"userId">> := BarUserId, <<"sessionId">> := BarSessionId},
     #{<<"userId">> := BazUserId,<<"sessionId">> := BazSessionId}] =
        create_users(SessionId, [<<"foo">>, <<"bar">>, <<"baz">>]),
    FooHeaders = [{"Cookie", webapp_client:bespoke_cookie(FooSessionId)}],
    _BarHeaders = [{"Cookie", webapp_client:bespoke_cookie(BarSessionId)}],
    _BazHeaders = [{"Cookie", webapp_client:bespoke_cookie(BazSessionId)}],
    %% Fetch all top messages
    {ok, []} = webapp_client:http_get("http://localhost/api/read_top_messages", FooHeaders),
    %% Create a message (without attachments: foo -> bar, baz)
    FooBody = unicode:characters_to_binary("BAJS\nPRUTTåäö\n"),
    FooBodyBlob = upload_blob(FooUserId, FooBody),
    BarBodyBlob = upload_blob(BarUserId, FooBody),
    BazBodyBlob = upload_blob(BazUserId, FooBody),
    {ok, #{<<"id">> := _MessageId}} =
        webapp_client:http_post("http://localhost/api/create_message",
                                #{<<"title">> => <<"A title">>,
                                  <<"bodyBlobs">> => [FooBodyBlob, BarBodyBlob, BazBodyBlob]},
                                FooHeaders),
    webapp_client:http_get("http://localhost/api/read_top_messages", FooHeaders).

create_users(_SessionId, []) ->
    [];
create_users(SessionId, [Username|Rest]) ->
    Headers = [{"Cookie", webapp_client:bespoke_cookie(SessionId)}],
    {ok, #{<<"sessionId">> := NewSessionId} = User} =
        webapp_client:http_post("http://localhost/api/switch_user",
                                #{<<"username">> => Username,
                                  <<"passwordSalt">> => null,
                                  <<"passwordHash">> => null,
                                  <<"clientResponse">> => null},
                                Headers),
    [User|create_users(NewSessionId, Rest)].

upload_blob(UserId, Data) ->
    #{<<"absPath">> := <<"/tmp/", Filename/binary>>} =
        webapp_client:http_multipart_post("http://localhost/api/upload_file", {data, Data}),
    #{<<"userId">> => UserId, <<"filename">> => filename:basename(Filename)}.
