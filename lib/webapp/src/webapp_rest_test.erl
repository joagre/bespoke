% -*- fill-column: 100; -*-

-module(webapp_rest_test).
-export([misc/0, messaging/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").

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
    ?log_info("** Auto login\n"),
    {ok, #{<<"noPassword">> := _NoPassword,
           <<"sessionId">> := SessionId,
           <<"userId">> := _UserId,
           <<"username">> := _Username}} =
        webapp_client:http_get("http://localhost/api/auto_login"),
    %% Create users
    ?log_info("** Create users\n"),
    [#{<<"userId">> := FooUserId, <<"sessionId">> := FooSessionId},
     #{<<"userId">> := BarUserId, <<"sessionId">> := BarSessionId},
     #{<<"userId">> := BazUserId,<<"sessionId">> := BazSessionId},
     #{<<"userId">> := _FuubarUserId,<<"sessionId">> := FuubarSessionId}] =
        create_users(SessionId, [<<"foo">>, <<"bar">>, <<"baz">>, <<"fuubar">>]),
    FooHeaders = [{"Cookie", webapp_client:bespoke_cookie(FooSessionId)}],
    BarHeaders = [{"Cookie", webapp_client:bespoke_cookie(BarSessionId)}],
    BazHeaders = [{"Cookie", webapp_client:bespoke_cookie(BazSessionId)}],
    FuubarHeaders = [{"Cookie", webapp_client:bespoke_cookie(FuubarSessionId)}],
    %% Fetch all top messages
    ?log_info("** Verify that foo has no top messages\n"),
    {ok, []} = webapp_client:http_get("http://localhost/api/read_top_messages", FooHeaders),
    %% Create a message (foo -> bar, baz) with two attachments
    ?log_info("** Create a message (foo -> bar, baz) with two attachments\n"),
    FooBody = <<"BAJS\nPRUTTåäö\n">>,
    FooBodyBlob = upload_blob(FooUserId, {data, FooBody}),
    BarBodyBlob = upload_blob(BarUserId, {data, FooBody}),
    BazBodyBlob = upload_blob(BazUserId, {data, FooBody}),
    ?log_info("** Create the two attachments\n"),
    FooAttachmentFilepath0 =
        filename:join([code:priv_dir(webapp), <<"docroot/images/animated-background.gif">>]),
    FooAttachmentBlob0 = upload_blob(FooUserId, FooAttachmentFilepath0),
    BarAttachmentBlob0 = upload_blob(BarUserId, FooAttachmentFilepath0),
    BazAttachmentBlob0 = upload_blob(BazUserId, FooAttachmentFilepath0),
    FooAttachmentFilepath1 = filename:join([code:priv_dir(webapp), <<"docroot/index.html">>]),
    FooAttachmentBlob1 = upload_blob(FooUserId, FooAttachmentFilepath1),
    BarAttachmentBlob1 = upload_blob(BarUserId, FooAttachmentFilepath1),
    BazAttachmentBlob1 = upload_blob(BazUserId, FooAttachmentFilepath1),
    ?log_info("** Create the message\n"),
    {ok, #{<<"id">> := _MessageId}} =
        webapp_client:http_post(
          "http://localhost/api/create_message",
          #{<<"title">> => <<"A title">>,
            <<"bodyBlobs">> => [FooBodyBlob, BarBodyBlob, BazBodyBlob],
            <<"attachmentBlobs">> =>
                [[FooAttachmentBlob0, BarAttachmentBlob0, BazAttachmentBlob0],
                 [FooAttachmentBlob1, BarAttachmentBlob1, BazAttachmentBlob1]]},
          FooHeaders),




    %% Check that created message landed on the server

    {ok, FooBodyString} = webapp_client:http_get("http://localhost/message/0/1"),
    FooBody = list_to_binary(FooBodyString),
    {ok, FooBodyString} = webapp_client:http_get("http://localhost/message/0/2"),
    FooBody = list_to_binary(FooBodyString),
    {ok, FooBodyString} = webapp_client:http_get("http://localhost/message/0/3"),
    FooBody = list_to_binary(FooBodyString),






    {ok, [#{<<"authorId">> := 1,
            <<"authorUsername">> := <<"foo">>,
            <<"id">> := 0,
            <<"title">> := <<"A title">>}]} =
        webapp_client:http_get("http://localhost/api/read_top_messages", FooHeaders),
    {ok, [#{<<"authorId">> := 1,
            <<"authorUsername">> := <<"foo">>,
            <<"id">> := 0,
            <<"title">> := <<"A title">>}]} =
        webapp_client:http_get("http://localhost/api/read_top_messages", BarHeaders),
    {ok, [#{<<"authorId">> := 1,
            <<"authorUsername">> := <<"foo">>,
            <<"id">> := 0,
            <<"title">> := <<"A title">>}]} =
        webapp_client:http_get("http://localhost/api/read_top_messages", BazHeaders),
    {ok, []} = webapp_client:http_get("http://localhost/api/read_top_messages", FuubarHeaders).

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

upload_blob(UserId, {data, Data}) ->
    #{<<"absPath">> := <<"/tmp/", Filename/binary>>} =
        webapp_client:http_multipart_post("http://localhost/api/upload_file", {data, Data}),
    #{<<"userId">> => UserId, <<"filename">> => filename:basename(Filename)};
upload_blob(UserId, FilePath) ->
    OriginFilename = filename:basename(FilePath),
    FilenameSize = byte_size(OriginFilename),
    ContentType = mimerl:extension(filename:extension(OriginFilename)),
    ContentTypeSize = byte_size(ContentType),
    Header = <<FilenameSize:32/unsigned-integer, OriginFilename:FilenameSize/binary,
               ContentTypeSize:32/integer, ContentType:ContentTypeSize/binary>>,
    TmpFilePath = webapp_client:prepend_file_header(FilePath, Header),
    #{<<"absPath">> := <<"/tmp/", Filename/binary>>} =
        webapp_client:http_multipart_post("http://localhost/api/upload_file", TmpFilePath),
    ok = file:delete(TmpFilePath),
    #{<<"userId">> => UserId, <<"filename">> => filename:basename(Filename)}.
