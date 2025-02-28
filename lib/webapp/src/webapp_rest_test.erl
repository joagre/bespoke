% -*- fill-column: 100; -*-

-module(webapp_rest_test).
-export([direct_messaging/0, forum/0, system/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").

%%
%% Exported: direct_messaging
%%

%% Run: make reset && ./bin/bespoke -- -eval "webapp_rest_test:direct_messaging()"

direct_messaging() ->
    _ = webapp_client:init_httpc(),
    ?log_info("**** Auto login"),
    {ok, #{<<"noPassword">> := _NoPassword,
           <<"sessionId">> := SessionId,
           <<"userId">> := _UserId,
           <<"username">> := _Username}} =
        webapp_client:http_get("http://localhost/api/auto_login"),
    ?log_info("**** Create users"),
    [#{<<"userId">> := FooUserId, <<"sessionId">> := FooSessionId},
     #{<<"userId">> := BarUserId, <<"sessionId">> := BarSessionId},
     #{<<"userId">> := BazUserId,<<"sessionId">> := BazSessionId},
     #{<<"userId">> := _FuubarUserId,<<"sessionId">> := FuubarSessionId}] =
        create_users(SessionId, [<<"foo">>, <<"bar">>, <<"baz">>, <<"fuubar">>]),
    FooHeaders = [{"Cookie", webapp_client:bespoke_cookie(FooSessionId)}],
    BarHeaders = [{"Cookie", webapp_client:bespoke_cookie(BarSessionId)}],
    BazHeaders = [{"Cookie", webapp_client:bespoke_cookie(BazSessionId)}],
    FuubarHeaders = [{"Cookie", webapp_client:bespoke_cookie(FuubarSessionId)}],
    ?log_info("**** Verify that foo has no top messages"),
    {ok, []} = webapp_client:http_get("http://localhost/api/read_top_messages", FooHeaders),
    ?log_info("**** Create message (foo -> bar, baz) with two attachments"),
    FooBody = <<"BAJS\nPRUTTåäö\n">>,
    FooBodyBlob = upload_blob(FooUserId, {data, FooBody}),
    BarBodyBlob = upload_blob(BarUserId, {data, FooBody}),
    BazBodyBlob = upload_blob(BazUserId, {data, FooBody}),
    ?log_info("**** Create two attachments"),
    FooAttachmentFilepath0 =
        filename:join([code:priv_dir(webapp), <<"docroot/images/animated-background.gif">>]),
    FooAttachmentBlob0 = upload_blob(FooUserId, FooAttachmentFilepath0),
    BarAttachmentBlob0 = upload_blob(BarUserId, FooAttachmentFilepath0),
    BazAttachmentBlob0 = upload_blob(BazUserId, FooAttachmentFilepath0),
    FooAttachmentFilepath1 = filename:join([code:priv_dir(webapp), <<"docroot/index.html">>]),
    FooAttachmentBlob1 = upload_blob(FooUserId, FooAttachmentFilepath1),
    BarAttachmentBlob1 = upload_blob(BarUserId, FooAttachmentFilepath1),
    BazAttachmentBlob1 = upload_blob(BazUserId, FooAttachmentFilepath1),
    ?log_info("**** Create top message"),
    {ok, #{<<"id">> := TopMessageId}} =
        webapp_client:http_post(
          "http://localhost/api/create_message",
          #{<<"bodyBlobs">> => [FooBodyBlob, BarBodyBlob, BazBodyBlob],
            <<"attachmentBlobs">> =>
                [[FooAttachmentBlob0, BarAttachmentBlob0, BazAttachmentBlob0],
                 [FooAttachmentBlob1, BarAttachmentBlob1, BazAttachmentBlob1]]},
          FooHeaders),
    ?log_info("**** Check body blobs"),
    {ok, _} = webapp_client:http_get("http://localhost/message/0/1"),
    {ok, _} = webapp_client:http_get("http://localhost/message/0/2"),
    {ok, _} = webapp_client:http_get("http://localhost/message/0/3"),
    ?log_info("**** Check attachment blobs"),
    {ok, _} = webapp_client:http_get("http://localhost/message/0/1-0"),
    {ok, _} = webapp_client:http_get("http://localhost/message/0/1-1"),
    {ok, _} = webapp_client:http_get("http://localhost/message/0/2-0"),
    {ok, _} = webapp_client:http_get("http://localhost/message/0/2-1"),
    {ok, _} = webapp_client:http_get("http://localhost/message/0/3-0"),
    {ok, _} = webapp_client:http_get("http://localhost/message/0/3-1"),
    ?log_info("**** Verify that foo, bar and baz got the top message"),
    {ok,[#{<<"attachmentIds">> := [0,1], <<"authorId">> := FooUserId,
           <<"authorUsername">> := <<"foo">>, <<"id">> := 0}]} =
        webapp_client:http_get("http://localhost/api/read_top_messages", FooHeaders),
    {ok,[#{<<"attachmentIds">> := [0,1], <<"authorId">> := FooUserId,
           <<"authorUsername">> := <<"foo">>, <<"id">> := 0}]} =
        webapp_client:http_get("http://localhost/api/read_top_messages", BarHeaders),
    {ok,[#{<<"attachmentIds">> := [0,1], <<"authorId">> := FooUserId,
           <<"authorUsername">> := <<"foo">>, <<"id">> := 0}]} =
        webapp_client:http_get("http://localhost/api/read_top_messages", BazHeaders),
    ?log_info("**** Verify that fuubar didn't get the top message"),
    {ok, []} = webapp_client:http_get("http://localhost/api/read_top_messages", FuubarHeaders),
    ?log_info("**** Create a reply as fuubar (should fail)"),
    {ok, 403, _, _, _} = webapp_client:http_post(
                           "http://localhost/api/create_message",
                           #{<<"topMessageId">> => TopMessageId,
                             <<"bodyBlobs">> =>  [FooBodyBlob, BarBodyBlob, BazBodyBlob]},
                           FuubarHeaders),
    ?log_info("**** Create a reply as baz (should *not* fail)"),
    BazBody = <<"BAJS\nPRUTTåäö\n">>,
    FooBodyBlob2 = upload_blob(FooUserId, {data, BazBody}),
    BarBodyBlob2 = upload_blob(BarUserId, {data, BazBody}),
    BazBodyBlob2 = upload_blob(BazUserId, {data, BazBody}),
    {ok, #{<<"id">> := BazMessageId}} =
        webapp_client:http_post(
          "http://localhost/api/create_message",
          #{<<"topMessageId">> => TopMessageId,
            <<"bodyBlobs">> => [FooBodyBlob2, BarBodyBlob2, BazBodyBlob2]},
          BazHeaders),
    ?log_info("**** Check body blobs"),
    {ok, _} = webapp_client:http_get("http://localhost/message/1/1"),
    {ok, _} = webapp_client:http_get("http://localhost/message/1/2"),
    {ok, _} = webapp_client:http_get("http://localhost/message/1/3"),
    ?log_info("**** Delete reply message"),
    {ok, 204, _, _, _} =
        webapp_client:http_post("http://localhost/api/delete_message", BazMessageId, BazHeaders),
    ?log_info("**** Delete top message"),
    {ok, 204, _, _, _} =
        webapp_client:http_post("http://localhost/api/delete_message", TopMessageId, FooHeaders).

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

%%
%% Exported: forum
%%

%% Run: make reset && ./bin/bespoke -- -eval "webapp_rest_test:forum()"

forum() ->
    _ = webapp_client:init_httpc(),
    ok = db_tools:create_subreddit_db(),
    ?log_info("**** Auto login"),
    {ok, #{<<"noPassword">> := _NoPassword,
           <<"sessionId">> := SessionId,
           <<"userId">> := _UserId,
           <<"username">> := _Username}} =
        webapp_client:http_get("http://localhost/api/auto_login"),
    ?log_info("**** Fetch all top posts"),
    Headers = [{"Cookie", webapp_client:bespoke_cookie(SessionId)}],
    {ok, [#{<<"id">> := PostId}|_]} =
        webapp_client:http_get("http://localhost/api/list_top_posts", Headers),
    ?log_info("**** Fetch specific post(s)"),
    {ok, [#{<<"id">> := PostId}]} =
        webapp_client:http_post("http://localhost/api/lookup_posts", [PostId], Headers),
    ?log_info("**** Fetch specific post(s) recursively (include all nested replies)"),
    {ok, [#{<<"id">> := _PostId2}|_]} =
        webapp_client:http_post("http://localhost/api/lookup_recursive_posts", [PostId],
                                Headers),
    ?log_info("**** Switch user"),
    {ok, #{<<"sessionId">> := NewSessionId,
           <<"userId">> := _NewUserId,
           <<"username">> := <<"foo">>}} =
        webapp_client:http_post("http://localhost/api/switch_user",
                                #{<<"username">> => <<"foo">>,
                                  <<"passwordSalt">> => null,
                                  <<"passwordHash">> => null,
                                  <<"clientResponse">> => null},
                                Headers),
    ?log_info("**** Insert a top post"),
    NewHeaders = [{"Cookie", webapp_client:bespoke_cookie(NewSessionId)}],
    {ok, #{<<"id">> := TopPostId}} =
        webapp_client:http_post("http://localhost/api/insert_post",
                                #{<<"title">> => <<"A new title for a top post">>,
                                  <<"body">> => <<"A body">>},
                                NewHeaders),
    ?log_info("**** Insert a reply post to the top post (including one attachment)"),
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
%% Exported: system
%%

%% Run: make reset && ./bin/bespoke -- -eval "webapp_rest_test:system()"

system() ->
    _ = webapp_client:init_httpc(),
    ?log_info("**** Auto login"),
    {ok, #{<<"noPassword">> := _NoPassword,
           <<"sessionId">> := SessionId,
           <<"userId">> := _UserId,
           <<"username">> := _Username}} =
        webapp_client:http_get("http://localhost/api/auto_login"),
    ?log_info("**** Switch user"),
    Headers = [{"Cookie", webapp_client:bespoke_cookie(SessionId)}],
    {ok, #{<<"sessionId">> := _NewSessionId,
           <<"userId">> := _NewUserId,
           <<"username">> := <<"foo">>}} =
        webapp_client:http_post("http://localhost/api/switch_user",
                                #{<<"username">> => <<"foo">>,
                                  <<"passwordSalt">> => null,
                                  <<"passwordHash">> => null,
                                  <<"clientResponse">> => null},
                                Headers).
