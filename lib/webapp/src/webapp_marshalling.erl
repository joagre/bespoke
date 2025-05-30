% -*- fill-column: 100; -*-

-module(webapp_marshalling).
-export([decode/2, encode/2]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("db/include/db.hrl").

-type decode_type() :: bootstrap |
                       save_settings |
                       generate_challenge |
                       login |
                       switch_user |
                       change_password |
                       change_admin_password |
                       create_message |
                       read_message |
                       read_reply_messages |
                       delete_message |
                       search_recipients |
                       create_file |
                       delete_file |
                       create_post |
                       read_posts|
                       read_recursive_posts |
                       read_recursive_post_ids |
                       delete_post |
                       toggle_post_like |
                       subscribe_on_changes |
                       mark_messages_as_read |
                       mark_posts_as_read.

-type encode_type() :: get_ssid |
                       generate_challenge |
                       login |
                       create_message |
                       read_message |
                       read_reply_messages |
                       read_top_messages |
                       delete_message |
                       search_recipients |
                       read_files |
                       create_file |
                       create_post |
                       read_top_posts |
                       read_posts |
                       read_recursive_posts |
                       read_recursive_post_ids |
                       toggle_post_like |
                       file_uploading.

%%
%% Exported: decode
%%

-spec decode(decode_type(), term()) -> {ok, term()} | {error, invalid}.

decode(bootstrap, #{<<"ssid">> := SSID}) when is_binary(SSID) ->
    {ok, SSID};
decode(save_settings, #{<<"bbsName">> := BBSName, <<"about">> := About})
  when is_binary(BBSName) andalso is_binary(About) ->
    {ok, {?b2l(BBSName), About}};
decode(generate_challenge, JsonTerm) ->
    decode_binary(JsonTerm);
decode(login, #{<<"username">> := Username, <<"clientResponse">> := ClientResponse} = JsonTerm)
  when is_binary(Username) andalso is_binary(ClientResponse) ->
    case valid_keys([<<"username">>, <<"clientResponse">>], JsonTerm) of
        true ->
            {ok, {Username, base64:decode(ClientResponse)}};
        false ->
            {error, invalid}
    end;
decode(switch_user, #{<<"username">> := Username,
                      <<"passwordSalt">> := PasswordSalt,
                      <<"passwordHash">> := PasswordHash,
                      <<"clientResponse">> := ClientResponse} = JsonTerm)
  when is_binary(Username) andalso
       (PasswordSalt == null orelse is_binary(PasswordSalt)) andalso
       (PasswordHash == null orelse is_binary(PasswordHash)) andalso
       (ClientResponse == null orelse is_binary(ClientResponse)) ->
    case valid_keys([<<"username">>,
                     <<"passwordSalt">>,
                     <<"passwordHash">>,
                     <<"clientResponse">>], JsonTerm) of
        true ->
            {ok, #{username => Username,
                   password_salt => base64decode(PasswordSalt),
                   password_hash => base64decode(PasswordHash),
                   client_response => base64decode(ClientResponse)}};
        false ->
            {error, invalid}
    end;
decode(DecodeType, #{<<"passwordSalt">> := PasswordSalt,
                     <<"passwordHash">> := PasswordHash} = JsonTerm)
  when (DecodeType == change_password orelse
        DecodeType == change_admin_password) andalso
       is_binary(PasswordHash) andalso is_binary(PasswordSalt) ->
    case valid_keys([<<"passwordSalt">>, <<"passwordHash">>], JsonTerm) of
        true ->
            {ok, {base64:decode(PasswordSalt), base64:decode(PasswordHash)}};
        false ->
            {error, invalid}
    end;
decode(create_message, JsonTerm) ->
    decode_message_bundle(JsonTerm);
decode(read_messages, JsonTerm) ->
    decode_integer_list(JsonTerm);
decode(read_reply_messages, JsonTerm) ->
    decode_integer(JsonTerm);
decode(delete_message, MessageId) ->
    decode_integer(MessageId);
decode(search_recipients , #{<<"ignoredUsernames">> := IgnoredUsernames,
                             <<"query">> := Query} = JsonTerm)
  when is_list(IgnoredUsernames) andalso is_binary(Query) ->
    case valid_keys([<<"ignoredUsernames">>, <<"query">>], JsonTerm) of
        true ->
            case decode_binary_list(IgnoredUsernames) of
                {ok, IgnoredUsernames} ->
                    {ok, {IgnoredUsernames, Query}};
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, invalid}
    end;
decode(create_file, JsonTerm) ->
    decode_file(JsonTerm);
decode(delete_file, JsonTerm) ->
    decode_integer(JsonTerm);
decode(file_is_uploaded, JsonTerm) ->
    decode_integer(JsonTerm);
decode(create_post, JsonTerm) ->
    decode_post(JsonTerm);
decode(read_posts, JsonTerm) ->
    decode_binary_list(JsonTerm);
decode(read_recursive_posts, JsonTerm) ->
    decode_binary_list(JsonTerm);
decode(read_recursive_post_ids, JsonTerm) ->
    decode_binary_list(JsonTerm);
decode(delete_post, JsonTerm) ->
    decode_binary(JsonTerm);
decode(toggle_post_like, JsonTerm) ->
    decode_binary(JsonTerm);
decode(subscribe_on_changes, JsonTerm) ->
    decode_binary_list(JsonTerm);
decode(mark_messages_as_read, JsonTerm) ->
    decode_integer_list(JsonTerm);
decode(mark_posts_as_read, JsonTerm) ->
    decode_binary_list(JsonTerm);
decode(_, _) ->
    {error, invalid}.

decode_message_bundle(#{<<"bodyBlobs">> := BodyBlobs} = JsonTerm) ->
    case valid_keys([<<"parentMessageId">>,
                     <<"topMessageId">>,
                     <<"bodyBlobs">>,
                     <<"attachmentBlobs">>], JsonTerm) of
        true ->
            ParentMessageId = maps:get(<<"parentMessageId">>, JsonTerm, not_set),
            TopMessageId = maps:get(<<"topMessageId">>, JsonTerm, not_set),
            AttachmentBlobs = maps:get(<<"attachmentBlobs">>, JsonTerm, []),
            maybe
                {ok, DecodedBodyBlobs} ?= decode_body_blobs(BodyBlobs),
                {ok, DecodedAttachmentBlobs} ?= decode_attachment_blobs(AttachmentBlobs),
                {ok, #{message => #message{parent_message_id = ParentMessageId,
                                           top_message_id = TopMessageId},
                       body_blobs => DecodedBodyBlobs,
                       attachment_blobs => DecodedAttachmentBlobs}}
            else
                Error ->
                    Error
            end;
        false ->
            {error, invalid}
    end.

decode_body_blobs(Blobs) ->
    decode_body_blobs(Blobs, []).

decode_body_blobs([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_body_blobs([#{<<"userId">> := UserId, <<"filename">> := Filename}|Rest], Acc)
  when is_integer(UserId), is_binary(Filename) ->
    decode_body_blobs(Rest, [{UserId, Filename}|Acc]);
decode_body_blobs(_, _) ->
    {error, invalid}.

decode_attachment_blobs(NestedBlobs) ->
    decode_attachment_blobs(NestedBlobs, []).

decode_attachment_blobs([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_attachment_blobs([Blobs|Rest], Acc) ->
    case decode_nested_attachment_blobs(Blobs) of
        {ok, DecodedBlobs} ->
            decode_attachment_blobs(Rest, [DecodedBlobs|Acc]);
        Error ->
            Error
    end;
decode_attachment_blobs(_, _) ->
    {error, invalid}.


decode_nested_attachment_blobs(Blobs) ->
    decode_nested_attachment_blobs(Blobs, []).

decode_nested_attachment_blobs([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_nested_attachment_blobs([#{<<"userId">> := UserId,
                                  <<"metadata">> := Metadata,
                                  <<"filename">> := Filename}|Rest], Acc)
  when is_integer(UserId), is_binary(Metadata), is_binary(Filename) ->
    decode_nested_attachment_blobs(Rest, [#{user_id => UserId,
                                            metadata => Metadata,
                                            filename => Filename}|Acc]);
decode_nested_attachment_blobs(_, _) ->
    {error, invalid}.

decode_file(#{<<"filename">> := Filename,
              <<"size">> := Size,
              <<"contentType">> := ContentType,
              <<"isUploading">> := IsUploading} = FileJsonTerm)
  when is_binary(Filename) andalso
       is_integer(Size) andalso
       is_binary(ContentType) andalso
       is_boolean(IsUploading) ->
    case valid_keys([<<"filename">>,
                     <<"size">>,
                     <<"contentType">>,
                     <<"isUploading">>], FileJsonTerm) of
        true ->
            {ok, #file{filename = Filename,
                       size = Size,
                       content_type = ContentType,
                       is_uploading = IsUploading}};
        false ->
            {error, invalid}
    end.

%% Top post
decode_post(#{<<"title">> := Title, <<"body">> := Body} = PostJsonTerm)
  when is_binary(Title) andalso is_binary(Body) ->
    case valid_keys([<<"title">>,
                     <<"body">>,
                     <<"created">>,
                     <<"attachments">>], PostJsonTerm) of
        true ->
            case {maps:get(<<"created">>, PostJsonTerm, not_set),
                  maps:get(<<"attachments">>, PostJsonTerm, [])} of
                {Created, AttachmentsJsonTerm}
                  when (Created == not_set orelse is_integer(Created)) andalso
                       (AttachmentsJsonTerm == [] orelse is_list(AttachmentsJsonTerm)) ->
                    case decode_attachments(AttachmentsJsonTerm) of
                        {ok, Attachments} ->
                            {ok, #post{title = Title,
                                       body = Body,
                                       created = Created,
                                       attachments = Attachments}};
                        {error, invalid} ->
                            {error, invalid}
                    end;
                _ ->
                    {error, invalid}
            end;
        false ->
            {error, invalid}
    end;
%% Reply post
decode_post(#{<<"parentPostId">> := ParentPostId,
              <<"topPostId">> := TopPostId,
              <<"body">> := Body} = PostJsonTerm)
  when is_binary(ParentPostId) andalso
       is_binary(TopPostId) andalso
       is_binary(Body) ->
    case valid_keys([<<"parentPostId">>,
                     <<"topPostId">>,
                     <<"body">>,
                     <<"created">>,
                     <<"attachments">>], PostJsonTerm) of
        true ->
            case {maps:get(<<"created">>, PostJsonTerm, not_set),
                  maps:get(<<"attachments">>, PostJsonTerm, [])} of
                {Created, AttachmentsJsonTerm}
                  when (Created == not_set orelse is_integer(Created)) andalso
                       (AttachmentsJsonTerm == [] orelse is_list(AttachmentsJsonTerm)) ->
                    case decode_attachments(AttachmentsJsonTerm) of
                        {ok, Attachments} ->
                            {ok, #post{parent_post_id = ParentPostId,
                                       top_post_id = TopPostId,
                                       body = Body,
                                       created = Created,
                                       attachments = Attachments}};
                        {error, invalid} ->
                            {error, invalid}
                    end;
                _ ->
                    {error, invalid}
            end;
        false ->
            {error, invalid}
    end;
decode_post(_) ->
    {error, invalid}.

decode_attachments(AttachmentsJsonTerm) ->
    decode_attachments(AttachmentsJsonTerm, []).

decode_attachments([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_attachments([AttachmentJsonTerm|Rest], Acc) ->
    case decode_attachment(AttachmentJsonTerm) of
        {ok, Attachment} ->
            decode_attachments(Rest, [Attachment|Acc]);
        {error, invalid} ->
            {error, invalid}
    end.

decode_attachment(#{<<"filename">> := Filename, <<"contentType">> := ContentType} = Attachment)
  when is_binary(Filename) andalso is_binary(ContentType) ->
    case valid_keys([<<"filename">>, <<"contentType">>], Attachment) of
        true ->
            {ok, {Filename, ContentType}};
        false ->
            {error, invalid}
    end;
decode_attachment(_) ->
    {error, invalid}.

%%
%% Exported: encode
%%

-spec encode(encode_type(), term()) -> term().

encode(get_ssid, SSID) ->
    ?l2b(SSID);
encode(generate_challenge, {PasswordSalt, Challenge}) ->
    #{<<"passwordSalt">> => base64:encode(PasswordSalt),
      <<"challenge">> => base64:encode(Challenge)};
encode(switch_user, #{bbs_name := BBSName,
                      user_id := UserId,
                      username := Username,
                      session_id := SessionId,
                      is_new := IsNew}) ->
    #{<<"bbsName">> => BBSName,
      <<"userId">> => UserId,
      <<"username">> => Username,
      <<"sessionId">> => base64:encode(SessionId),
      <<"isNew">> => IsNew};
encode(login, #{bbs_name := BBSName,
                user_id := UserId,
                username := Username,
                session_id := SessionId}) ->
    #{<<"bbsName">> => BBSName,
      <<"userId">> => UserId,
      <<"username">> => Username,
      <<"sessionId">> => base64:encode(SessionId)};
encode(create_message, Message) ->
    encode_message(Message);
encode(read_top_messages, MessageBundles) ->
    encode_message_bundles(MessageBundles);
encode(read_messages, MessageBundles) ->
    encode_message_bundles(MessageBundles);
encode(read_reply_messages, MessageBundles) ->
    encode_message_bundles(MessageBundles);
encode(search_recipients, Recipients) ->
    encode_recipients(Recipients);
encode(create_post, {Post, ReadPostIds}) ->
    encode_post(Post, ReadPostIds);
encode(create_file, File) ->
    encode_file(File);
encode(read_files, Files) ->
    lists:map(fun(File) -> encode_file(File) end, Files);
encode(read_top_posts, {ReadPostIds, TopPosts}) ->
    lists:map(fun({Post, ReadCount}) ->
                      PostJsonTerm = encode_post(Post, ReadPostIds),
                      maps:put(<<"readCount">>, ReadCount, PostJsonTerm)
              end, TopPosts);
encode(read_posts, {ReadPostIds, Posts}) ->
    lists:map(fun(Post) -> encode_post(Post, ReadPostIds) end, Posts);
encode(read_recursive_posts, {ReadPostIds, Posts}) ->
    lists:map(fun(Post) -> encode_post(Post, ReadPostIds) end, Posts);
encode(read_recursive_post_ids, PostIds) ->
    PostIds;
encode(toggle_post_like, {UserId, Likers}) ->
    #{<<"liked">> => lists:member(UserId, Likers),
      <<"likesCount">> => length(Likers)};
encode(upload_file, #{filename := Filename,
                      absPath := AbsPath,
                      contentType := ContentType}) ->
    #{<<"filename">> => Filename,
      <<"absPath">> => AbsPath,
      <<"contentType">> => ContentType}.

encode_message(#message{id = Id,
                        parent_message_id = ParentMessageId,
                        top_message_id = TopMessageId,
                        author = AuthorId,
                        created = Created}) ->
    {ok, #user{name = AuthorUsername}} = db_user_serv:get_user(AuthorId),
    JsonTerm = #{<<"id">> => Id,
                 <<"authorId">> => AuthorId,
                 <<"authorUsername">> => AuthorUsername,
                 <<"created">> => Created},
    add_optional_members([{<<"parentMessageId">>, ParentMessageId},
                          {<<"topMessageId">>, TopMessageId}], JsonTerm).

encode_message_bundles(MessagesBundles) ->
    lists:map(fun(#{message := Message,
                    attachment_ids := AttachmentIds,
                    reply_message_ids := ReplyMessageIds,
                    recipients := Recipients,
                    read_count := ReadCount,
                    is_read := IsRead}) ->
                      EncodedMessage = encode_message(Message),
                      EncodedMessage#{<<"attachmentIds">> => AttachmentIds,
                                      <<"replyMessageIds">> => ReplyMessageIds,
                                      <<"recipients">> => encode_recipients(Recipients),
                                      <<"readCount">> => ReadCount,
                                      <<"isRead">> => IsRead};
                 (#{message := Message,
                    attachment_ids := AttachmentIds,
                    is_read := IsRead}) ->
                      EncodedMessage = encode_message(Message),
                      EncodedMessage#{<<"attachmentIds">> => AttachmentIds, <<"isRead">> => IsRead}
              end, MessagesBundles).

encode_recipients([]) ->
    [];
encode_recipients([Recipient|Rest]) ->
    [encode_recipient(Recipient)|encode_recipients(Rest)].

encode_recipient(#{user_id := UserId,
                   username := Username,
                   ignored := Ignored}) ->
    #{<<"userId">> => UserId,
      <<"username">> => Username,
      <<"ignored">> => Ignored};
encode_recipient(#{user_id := UserId,
                   username := Username}) ->
    #{<<"userId">> => UserId, <<"username">> => Username}.

encode_post(#post{id = Id,
                  title = Title,
                  parent_post_id = ParentPostId,
                  top_post_id = TopPostId,
                  body = Body,
                  author = AuthorId,
                  created = Created,
                  reply_count = ReplyCount,
                  replies = Replies,
                  likers = Likers,
                  attachments = AttachmentsJsonTerm}, ReadPostIds) ->
    {ok, #user{name = AuthorUsername}} = db_user_serv:get_user(AuthorId),
    JsonTerm = #{<<"id">> => Id,
                 <<"body">> => Body,
                 <<"authorId">> => AuthorId,
                 <<"authorUsername">> => AuthorUsername,
                 <<"created">> => Created,
                 <<"replyCount">> => ReplyCount,
                 <<"replies">> => Replies,
                 <<"likers">> => Likers,
                 <<"attachments">> => encode_attachments(AttachmentsJsonTerm),
                 <<"isRead">> => lists:member(Id, ReadPostIds)},
    add_optional_members([{<<"title">>, Title},
                          {<<"parentPostId">>, ParentPostId},
                          {<<"topPostId">>, TopPostId}], JsonTerm).

encode_attachments([]) ->
    [];
encode_attachments([Attachment|Rest]) ->
    [encode_attachment(Attachment)|encode_attachments(Rest)].

encode_attachment({Filename, ContentType}) ->
    #{<<"filename">> => Filename,
      <<"contentType">> => ContentType}.

encode_file(#file{id = Id,
                  filename = Filename,
                  size = Size,
                  uploaded_size = UploadedSize,
                  content_type = ContentType,
                  uploader = UploaderId,
                  created = Created,
                  is_uploading = IsUploading}) ->
    {ok, #user{name = UploaderUsername}} = db_user_serv:get_user(UploaderId),
    #{<<"id">> => Id,
      <<"filename">> => Filename,
      <<"size">> => Size,
      <<"uploadedSize">> => UploadedSize,
      <<"contentType">> => ContentType,
      <<"uploaderId">> => UploaderId,
      <<"uploaderUsername">> => UploaderUsername,
      <<"created">> => Created,
      <<"isUploading">> => IsUploading}.

%%
%% Utilities
%%

valid_keys(PossibleKeys, Map) ->
    lists:all(fun(Key) -> lists:member(Key, PossibleKeys) end, maps:keys(Map)).

add_optional_members([], JsonTerm) ->
    JsonTerm;
add_optional_members([{_Key, not_set}|Rest], JsonTerm) ->
    add_optional_members(Rest, JsonTerm);
add_optional_members([{Key, Value}|Rest], JsonTerm) ->
    add_optional_members(Rest, maps:put(Key, Value, JsonTerm)).

base64decode(null) ->
    not_set;
base64decode(Value) ->
    base64:decode(Value).

decode_integer_list(List) when is_list(List) ->
    decode_integer_list(List, []);
decode_integer_list(_) ->
    {error, invalid}.

decode_integer_list([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_integer_list([Int|Rest], Acc) when is_integer(Int) ->
    decode_integer_list(Rest, [Int|Acc]);
decode_integer_list(_, _) ->
    {error, invalid}.

decode_integer(Int) when is_integer(Int) ->
    {ok, Int};
decode_integer(_) ->
    {error, invalid}.

decode_binary(Bin) when is_binary(Bin) ->
    {ok, Bin};
decode_binary(_) ->
    {error, invalid}.

decode_binary_list(List) when is_list(List) ->
    decode_binary_list(List, []);
decode_binary_list(_) ->
    {error, invalid}.

decode_binary_list([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_binary_list([Bin|Rest], Acc) when is_binary(Bin) ->
    decode_binary_list(Rest, [Bin|Acc]);
decode_binary_list(_, _Acc) ->
    {error, invalid}.
