% -*- fill-column: 100; -*-

-module(db_message_db).
-export([open/0, dump/0, sync/0, close/0, create_message/3, read_top_messages/1,
         read_messages/1, read_reply_messages/2, delete_message/2]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("../include/db.hrl").

%% Message DB
-define(MESSAGE_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "message.db")).
-define(MESSAGE_DB, message_db).

%% Top Message DB (user-id -> [top-message-id, ...])
%% Description: To quickly find all top messages for a user
-define(TOP_MESSAGE_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "top_message.db")).
-define(TOP_MESSAGE_DB, top_message_db).

%% Reply Message DB (top-message-id -> [message-id, ...])
%% Description: To quickly find all reply messages to a top message
-define(REPLY_MESSAGE_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "reply_message.db")).
-define(REPLY_MESSAGE_DB, reply_message_db).

%% Recipient DB (top-message-id -> [user-id, ...])
%% Description: To quickly find all recipients to a top message
-define(RECIPIENT_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "recipient.db")).
-define(RECIPIENT_DB, recipient_db).

%% Attachment DB (message-id -> [attachment-id, ...])
%% Description: To quickly find all attachments to a message
-define(ATTACHMENT_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "attachment.db")).
-define(ATTACHMENT_DB, attachment_db).

%%
%% Exported: open
%%

-spec open() -> ok | {error, file:posix()}.

open() ->
    maybe
        {ok, _} ?= dets:open_file(?MESSAGE_DB, [{file, ?MESSAGE_FILE_PATH}, {keypos, #message.id}]),
        {ok, _} ?= idets:open_file(?TOP_MESSAGE_DB, ?TOP_MESSAGE_FILE_PATH),
        {ok, _} ?= idets:open_file(?REPLY_MESSAGE_DB, ?REPLY_MESSAGE_FILE_PATH),
        {ok, _} ?= idets:open_file(?RECIPIENT_DB, ?RECIPIENT_FILE_PATH),
        {ok, _} ?= idets:open_file(?ATTACHMENT_DB, ?ATTACHMENT_FILE_PATH),
        ok
    end.

%%
%% Exported: dump
%%

-spec dump() -> [{dets:tab_name(), [term()]}].

dump() ->
    [{?MESSAGE_DB, db:dets_dump(?MESSAGE_DB)},
     {?TOP_MESSAGE_DB, idets:dump(?TOP_MESSAGE_DB)},
     {?REPLY_MESSAGE_DB, idets:dump(?REPLY_MESSAGE_DB)},
     {?RECIPIENT_DB, idets:dump(?RECIPIENT_DB)},
     {?ATTACHMENT_DB, idets:dump(?ATTACHMENT_DB)}].

%%
%% Exported: sync
%%

-spec sync() -> ok | {error, term()}.

sync() ->
    maybe
        ok ?= dets:sync(?MESSAGE_DB),
        ok ?= idets:sync(?TOP_MESSAGE_DB),
        ok ?= idets:sync(?REPLY_MESSAGE_DB),
        ok ?= idets:sync(?RECIPIENT_DB),
        ok ?= idets:sync(?ATTACHMENT_DB)
    end.

%%
%% Exported: close
%%

-spec close() -> ok | {error, term()}.

close() ->
    maybe
        ok ?= dets:close(?MESSAGE_DB),
        ok ?= idets:close(?TOP_MESSAGE_DB),
        ok ?= idets:close(?REPLY_MESSAGE_DB),
        ok ?= idets:close(?RECIPIENT_DB),
        ok ?= idets:close(?ATTACHMENT_DB)
    end.

%%
%% Exported: create_message
%%

-spec create_message(#message{},
                     [{db:user_id(), main:filename()}],
                     [[#{user_id => db:user_id(),
                         metadata => main:filename(),
                         filename => main:filename()}]]) ->
          {ok, #message{}} | {error, file:posix() | access_denied}.

create_message(#message{top_message_id = TopMessageId, author = Author} = Message,
               BodyBlobs, AttachmentBlobs) ->
    RecipientMessageIds =
        case TopMessageId of
            %% Is a top message!
            not_set ->
                [];
            %% Is a reply message!
            _ ->
                idets:lookup(?RECIPIENT_DB, TopMessageId)
        end,
    case TopMessageId == not_set orelse lists:member(Author, RecipientMessageIds) of
        true ->
            MessageId = db_meta_db:read_next_message_id(),
            UpdatedMessage = Message#message{id = MessageId,
                                             created = db:seconds_since_epoch()},

            %% Update MESSAGE_DB
            ok = dets:insert(?MESSAGE_DB, UpdatedMessage),
            case create_blobs(UpdatedMessage, BodyBlobs, AttachmentBlobs) of
                ok ->
                    {ok, UpdatedMessage};
                Error ->
                    Error
            end;
        false ->
            {error, access_denied}
    end.

create_blobs(#message{id = MessageId} = Message, BodyBlobs, AttachmentBlobs) ->
    maybe
        %% Note: Disk layout is described in db.hrl
        MessageBlobPath = filename:join([?BESPOKE_MESSAGE_PATH, ?i2b(MessageId)]),
        ok ?= make_dir(MessageBlobPath),
        ok ?= create_body_blobs(Message, MessageBlobPath, BodyBlobs),
        ok ?= create_attachment_blobs(Message, MessageBlobPath, AttachmentBlobs)
    end.

create_body_blobs(_Message, _MessageBlobPath, []) ->
    ok;
create_body_blobs(#message{id = MessageId, top_message_id = TopMessageId} = Message,
                  MessageBlobPath, [{UserId, BlobFilename}|Rest]) ->
    CurrentBlobPath = filename:join([?BESPOKE_TMP_PATH, BlobFilename]),
    NewBlobPath = filename:join([MessageBlobPath, ?i2b(UserId)]),
    ?log_info("Renaming body blob from ~s to ~s", [CurrentBlobPath, NewBlobPath]),
    case file:rename(CurrentBlobPath, NewBlobPath) of
        ok ->
            case TopMessageId of
                %% Is a top message!
                not_set ->
                    %% Update TOP_MESSAGE_DB and RECIPIENT_DB
                    ok = idets:insert(?TOP_MESSAGE_DB, UserId, MessageId),
                    ok = idets:insert(?RECIPIENT_DB, MessageId, UserId);
                %% Is a reply message!
                TopMessageId ->
                    %% Update REPLY_MESSAGE_DB and RECIPIENT_DB
                    ok = idets:insert(?REPLY_MESSAGE_DB, TopMessageId, MessageId),
                    ok = idets:insert(?RECIPIENT_DB, TopMessageId, UserId)
            end,
            create_body_blobs(Message, MessageBlobPath, Rest);
        {error, Reason} ->
            {error, Reason}
    end.

create_attachment_blobs(_Message, _MessageBlobPath, []) ->
    ok;
create_attachment_blobs(Message, MessageBlobPath, [AttachmentBlobs|Rest]) ->
    AttachmentId = db_meta_db:read_next_attachment_id(),
    case create_attachment_blobs(AttachmentId, Message, MessageBlobPath, AttachmentBlobs) of
        ok ->
            create_attachment_blobs(Message, MessageBlobPath, Rest);
        {error, Reason} ->
            {error, Reason}
    end.

create_attachment_blobs(_AttachmentId, _Message, _MessageBlobPath, []) ->
    ok;
create_attachment_blobs(AttachmentId, #message{id = MessageId} = Message, MessageBlobPath,
                        [#{user_id := UserId,
                           metadata := MetadataFilename,
                           filename := BlobFilename}|Rest]) ->
    %% Rename attachment blob
    CurrentBlobPath = filename:join([?BESPOKE_TMP_PATH, BlobFilename]),
    NewBlobPath = filename:join([MessageBlobPath, io_lib:format("~w-~w", [UserId, AttachmentId])]),
    ?log_info("Renaming attachment blob ~s to ~s", [CurrentBlobPath, NewBlobPath]),
    case file:rename(CurrentBlobPath, NewBlobPath) of
        ok ->
            %% Update ATTACHMENT_DB
            ok = idets:insert(?ATTACHMENT_DB, MessageId, AttachmentId),

            %% Rename metadata blob
            CurrentMetadataBlobPath = filename:join([?BESPOKE_TMP_PATH, MetadataFilename]),
            NewMetadataBlobPath =
                filename:join([MessageBlobPath,
                               io_lib:format("~w-~w.dat", [UserId, AttachmentId])]),
            ?log_info("Renaming attachment metadata blob ~s to ~s",
                      [CurrentMetadataBlobPath, NewMetadataBlobPath]),
            ok = file:rename(CurrentMetadataBlobPath, NewMetadataBlobPath),
            create_attachment_blobs(AttachmentId, Message, MessageBlobPath, Rest);
        {error, Reason} ->
            {error, Reason}
    end.

%%
%% Exported: read_top_messages
%%

-spec read_top_messages(db:user_id()) -> {ok, [#{message => #message{},
                                                 attachment_ids => [db:attachment_id()],
                                                 reply_message_ids => [db:message_id()],
                                                 recipients => [#{user_id => db:user_id(),
                                                                  username => db:username()}]}]}.

read_top_messages(UserId) ->
    TopMessageIds = idets:lookup(?TOP_MESSAGE_DB, UserId),
    {ok, TopMessages} = read_messages(TopMessageIds),
    {ok, reverse_sort_messages(TopMessages)}.

%%
%% Exported: read_messages
%%

-spec read_messages([db:message_id()]) -> {ok, [#{message => #message{},
                                                  attachment_ids => [db:attachment_id()],
                                                  reply_message_ids => [db:message_id()],
                                                  recipients => [#{user_id => db:user_id(),
                                                                   username => db:username()}]}]}.

read_messages(MessageIds) ->
    Messages = lookup_messages(MessageIds),
    MessageBundles =
        lists:map(fun(#message{id = MessageId} = Message) ->
                          AttachmentIds = idets:lookup(?ATTACHMENT_DB, MessageId),
                          #{message => Message, attachment_ids => AttachmentIds}
                  end, Messages),
    {ok, add_top_message_info(MessageBundles)}.

lookup_messages([]) ->
    [];
lookup_messages([MessageId|Rest]) ->
    [Message] = dets:lookup(?MESSAGE_DB, MessageId),
    [Message|lookup_messages(Rest)].

add_top_message_info(MessageBundles) ->
    lists:map(
      fun(#{message := #message{id = MessageId, top_message_id = not_set}} = MessageBundle) ->
              ReplyMessageIds = idets:lookup(?REPLY_MESSAGE_DB, MessageId),
              RecipientUserIds = idets:lookup(?RECIPIENT_DB, MessageId),
              Recipients = lists:map(
                             fun(UserId) ->
                                     {ok, #user{name = Username}} = db_user_serv:get_user(UserId),
                                     #{user_id => UserId, username => Username}
                             end, RecipientUserIds),
              MessageBundle#{reply_message_ids => ReplyMessageIds,
                             recipients => Recipients};
         (MessageBundle) ->
              MessageBundle
      end, MessageBundles).

%%
%% Exported: read_reply_messages
%%

-spec read_reply_messages(db:user_id(), db:message_id()) ->
          {ok, [#{message => #message{},
                  attachment_ids => [db:attachment_id()]}]} |
          {error, access_denied}.

read_reply_messages(UserId, TopMessageId) ->
    RecipientMessageIds = idets:lookup(?RECIPIENT_DB, UserId),
    [TopMessage] = dets:lookup(?MESSAGE_DB, TopMessageId),
    case UserId == TopMessage#message.author orelse
        lists:member(TopMessageId, RecipientMessageIds) of
        true ->
            MessageIds = idets:lookup(?REPLY_MESSAGE_DB, TopMessageId),
            {ok, ReplyMessages} = read_messages(MessageIds),
            {ok, sort_messages(ReplyMessages)};
        false ->
            {error, access_denied}
    end.

%%
%% Exported: delete_message
%%

-spec delete_message(db:user_id(), db:message_id()) -> ok | {error, access_denied}.

delete_message(UserId, MessageId) ->
    case dets:lookup(?MESSAGE_DB, MessageId) of
        %% Is a top message!
        [#message{top_message_id = not_set, author = UserId} = Message] ->
            %% Remove all reply messages
            ReplyMessageIds = idets:lookup(?REPLY_MESSAGE_DB, MessageId),
            lists:foreach(fun(ReplyMessageId) ->
                                  [ReplyMessage] = dets:lookup(?MESSAGE_DB, ReplyMessageId),
                                  ok = delete_message(ReplyMessage)
                          end, ReplyMessageIds),

            %% Remove top message
            ok = delete_message(Message);
        %% Is a reply message!
        [#message{author = UserId} = Message] ->
            ok = delete_message(Message);
        _ ->
            {error, access_denied}
    end.

%% Is top message!
delete_message(#message{id = MessageId, top_message_id = not_set} = Message) ->
    ok = delete_blobs(Message),

    %% Update MESSAGE_DB
    ok = dets:delete(?MESSAGE_DB, MessageId),

    %% Update TOP_MESSAGE_DB
    RecipientUserIds = idets:lookup(?RECIPIENT_DB, MessageId),


    lists:foreach(
      fun(RecipientUserId) ->
              ok = idets:delete(?TOP_MESSAGE_DB, RecipientUserId, MessageId)
      end, RecipientUserIds),

    ?log_info("TOP_MESSAGE_DB after delete: ~p", [idets:dump(?TOP_MESSAGE_DB)]),


    %% Update REPLY_MESSAGE_DB
    ok = idets:delete(?REPLY_MESSAGE_DB, MessageId),

    %% Update RECIPIENT_DB
    ok = idets:delete(?RECIPIENT_DB, MessageId),

    %% Update ATTACHMENT_DB
    AttachmentIds = idets:lookup(?ATTACHMENT_DB, MessageId),
    lists:foreach(fun(AttachmentId) ->
                          ok = idets:delete(?ATTACHMENT_DB, AttachmentId)
                  end, AttachmentIds);
%% Is reply message!
delete_message(#message{id = MessageId, top_message_id = TopMessageId} = Message) ->
    ok = delete_blobs(Message),

    %% Update REPLY_MESSAGE_DB
    ok = idets:delete(?REPLY_MESSAGE_DB, TopMessageId, MessageId),

    %% Update ATTACHMENT_DB
    AttachmentIds = idets:lookup(?ATTACHMENT_DB, MessageId),
    lists:foreach(fun(AttachmentId) ->
                          ok = idets:delete(?ATTACHMENT_DB, AttachmentId)
                  end, AttachmentIds);
delete_message(_Message) ->
    {error, access_denied}.

delete_blobs(#message{id = MessageId, top_message_id = TopMessageId}) ->
    RecipientUserIds =
        case TopMessageId of
            %% Is a top message!
            not_set ->
                idets:lookup(?RECIPIENT_DB, MessageId);
            %% Is a reply message!
            _ ->
                idets:lookup(?RECIPIENT_DB, TopMessageId)
        end,
    MessageBlobPath = filename:join([?BESPOKE_MESSAGE_PATH, ?i2b(MessageId)]),
    lists:foreach(
      fun(RecipientUserId) ->
              BodyBlobPath = filename:join([MessageBlobPath, ?i2b(RecipientUserId)]),
              ?log_info("Deleting body blob ~s", [BodyBlobPath]),
              case file:delete(BodyBlobPath) of
                  ok ->
                      AttachmentIds = idets:lookup(?ATTACHMENT_DB, MessageId),
                      lists:foreach(
                        fun(AttachmentId) ->
                                AttachmentBlobPath =
                                    filename:join([MessageBlobPath,
                                                   io_lib:format("~w-~w",
                                                                 [RecipientUserId, AttachmentId])]),
                                ?log_info("Deleting attachment blob ~s", [AttachmentBlobPath]),
                                case file:delete(AttachmentBlobPath) of
                                    ok ->
                                        ok;
                                    {error, Reason} ->
                                        ?log_error("Failed to delete ~s: ~s",
                                                   [AttachmentBlobPath, Reason])
                                end
                        end, AttachmentIds);
                  {error, Reason} ->
                      ?log_error("Failed to delete ~s: ~s", [BodyBlobPath, Reason]),
                      %% Note: A recipient added late to a thread may not have a body blob
                      ok
              end
      end, RecipientUserIds).

%%
%% Utilities
%%

sort_messages(Messages) ->
    lists:sort(fun(MessageBundleA, MessageBundleB) ->
                       #message{created= CreatedA} = maps:get(message, MessageBundleA),
                       #message{created= CreatedB} = maps:get(message, MessageBundleB),
                       CreatedA < CreatedB
               end, Messages).

reverse_sort_messages(Messages) ->
    lists:sort(fun(MessageBundleA, MessageBundleB) ->
                       #message{created= CreatedA} = maps:get(message, MessageBundleA),
                       #message{created= CreatedB} = maps:get(message, MessageBundleB),
                       CreatedA > CreatedB
               end, Messages).

make_dir(DirPath) ->
    case filelib:is_dir(DirPath) of
        true ->
            ok;
        false ->
            file:make_dir(DirPath)
    end.
