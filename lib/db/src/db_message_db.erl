% -*- fill-column: 100; -*-

-module(db_message_db).
-export([open/0, sync/0, close/0, create_message/3, read_top_messages/1, read_reply_messages/2,
         delete_message/2]).

-include_lib("apptools/include/shorthand.hrl").
-include("../include/db.hrl").
-include("db_message_db.hrl").

%%
%% Exported: open
%%

-spec open() -> ok | {error, file:posix()}.

open() ->
    maybe
        {ok, _} ?= db:open_disk_db(?MESSAGE_DB, ?MESSAGE_FILE_PATH, #message.id),
        {ok, _} ?= db:open_disk_index_db(?MESSAGE_INDEX_DB, ?MESSAGE_INDEX_FILE_PATH),
        {ok, _} ?= db:open_disk_db(?MESSAGE_RECIPIENT_DB, ?MESSAGE_RECIPIENT_FILE_PATH,
                                   #message_recipient.message_id),
        {ok, _} ?= db:open_disk_index_db(?MESSAGE_RECIPIENT_INDEX_DB,
                                         ?MESSAGE_RECIPIENT_INDEX_FILE_PATH),
        {ok, _} ?= db:open_disk_db(?MESSAGE_ATTACHMENT_DB, ?MESSAGE_ATTACHMENT_FILE_PATH,
                                   #message_attachment.id),
        {ok, _} ?= db:open_disk_index_db(?MESSAGE_ATTACHMENT_INDEX_DB,
                                         ?MESSAGE_ATTACHMENT_INDEX_FILE_PATH),
        ok
    else
        Error ->
            Error
    end.

%%
%% Exported: sync
%%

-spec sync() -> ok | {error, term()}.

sync() ->
    maybe
        ok ?= db:sync_disk_index_db(?MESSAGE_ATTACHMENT_INDEX_DB),
        ok ?= db:sync_disk_db(?MESSAGE_ATTACHMENT_DB),
        ok ?= db:sync_disk_index_db(?MESSAGE_RECIPIENT_INDEX_DB),
        ok ?= db:sync_disk_db(?MESSAGE_RECIPIENT_DB),
        ok ?= db:sync_disk_index_db(?MESSAGE_INDEX_DB),
        ok ?= db:sync_disk_db(?MESSAGE_DB)
    else
        Error ->
            Error
    end.

%%
%% Exported: close
%%

-spec close() -> ok | {error, term()}.

close() ->
    maybe
        ok ?= db:close_disk_index_db(?MESSAGE_ATTACHMENT_INDEX_DB),
        ok ?= db:close_disk_db(?MESSAGE_ATTACHMENT_DB),
        ok ?= db:close_disk_index_db(?MESSAGE_RECIPIENT_INDEX_DB),
        ok ?= db:close_disk_db(?MESSAGE_RECIPIENT_DB),
        ok ?= db:close_disk_index_db(?MESSAGE_INDEX_DB),
        ok ?= db:close_disk_db(?MESSAGE_DB)
    else
        Error ->
            Error
    end.

%%
%% Exported: create_message
%%

-spec create_message(#message{},
                     [{db_serv:user_id(), main:filename()}],
                     [[{db_serv:user_id(), main:filename()}]]) ->
          {ok, #message{}} | {error, term()}.

create_message(Message, BodyBlobs, AttachmentBlobs) ->
    MessageId = db_meta_db:read_next_message_id(),
    case handle_blobs(MessageId, BodyBlobs, AttachmentBlobs) of
        ok ->
            UpdatedMessage =
                Message#message{
                  id = MessageId,
                  created = db:seconds_since_epoch()
                 },
            ok = dets:insert(?MESSAGE_DB, UpdatedMessage),
            {ok, UpdatedMessage};
        Error ->
            Error
    end.

handle_blobs(MessageId, BodyBlobs, AttachmentBlobs) ->
    maybe
        %% Note: Disk layout is described in db.hrl
        MessageBlobPath = filename:join([?BESPOKE_MESSAGE_PATH, ?i2b(MessageId)]),
        ok ?= file:make_dir(MessageBlobPath),
        ok ?= handle_body_blobs(MessageId, MessageBlobPath, BodyBlobs),
        ok ?= handle_attachment_blobs(MessageId, MessageBlobPath, AttachmentBlobs)
    else
        Error ->
            Error
    end.

handle_body_blobs(_MessageId, _MessageBlobPath, []) ->
    ok;
handle_body_blobs(MessageId, MessageBlobPath, [{UserId, BlobFilename}|Rest]) ->
    CurrentBlobPath = filename:join([?BESPOKE_TMP_PATH, BlobFilename]),
    NewBlobPath = filename:join([MessageBlobPath, ?i2b(UserId)]),
    ok = file:rename(CurrentBlobPath, NewBlobPath),
    ok = dets:insert(?MESSAGE_RECIPIENT_DB,
                     #message_recipient{message_id = MessageId, user_id = UserId}),
    handle_body_blobs(MessageId, MessageBlobPath, Rest).

handle_attachment_blobs(_MessageId, _MessageBlobPath, []) ->
    ok;
handle_attachment_blobs(MessageId, MessageBlobPath, [AttachmentBlobs|Rest])
  when is_list(AttachmentBlobs) ->
    ok = handle_attachment_blobs(MessageId, MessageBlobPath, AttachmentBlobs),
    handle_attachment_blobs(MessageId, MessageBlobPath, Rest);
handle_attachment_blobs(MessageId, MessageBlobPath, [{UserId, BlobFilename}|Rest]) ->
    CurrentBlobPath = filename:join([?BESPOKE_TMP_PATH, BlobFilename]),
    AttachmentId = db_meta_db:read_next_message_attachment_id(),
    NewBlobPath = filename:join([MessageBlobPath, io_lib:format("~w-~w", [UserId, AttachmentId])]),
    ok = file:rename(CurrentBlobPath, NewBlobPath),
    ok = dets:insert(?MESSAGE_ATTACHMENT_DB,
                     #message_attachment{id = AttachmentId, message_id = MessageId}),
    handle_attachment_blobs(MessageId, MessageBlobPath, Rest).

%%
%% Exported: read_top_messages
%%

-spec read_top_messages(db_serv:user_id()) -> {ok, [#message{}]}.

read_top_messages(UserId) ->
    MessageIds = dets:lookup(?MESSAGE_RECIPIENT_INDEX_DB, UserId),
    Messages =
        lists:foldl(
          fun(MessageId, Acc) ->
                  case dets:lookup(?MESSAGE_DB, MessageId) of
                      [#message{top_message_id = not_set} = Message] ->
                          [Message|Acc];
                      _ ->
                          Acc
                  end
          end, [], MessageIds),
    {ok, sort(Messages)}.

%%
%% Exported: read_reply_messages
%%

-spec read_reply_messages(db_serv:user_id(), db_serv:message_id()) ->
          {ok, [#message{}]} | {error, access_denied}.

read_reply_messages(UserId, TopMessageId) ->
    RecipientMessageIds = dets:lookup(?MESSAGE_RECIPIENT_DB, UserId),
    case lists:member(TopMessageId, RecipientMessageIds) of
        true ->
            MessageIds = dets:lookup(?MESSAGE_INDEX_DB, TopMessageId),
            Messages =
                lists:foldl(fun(MessageId, Acc) ->
                                    [Message] = dets:lookup(?MESSAGE_DB, MessageId),
                                    [Message|Acc]
                            end, [], MessageIds),
            {ok, sort(Messages)};
        false ->
            {error, access_denied}
    end.

%%
%% Exported: delete_message
%%

-spec delete_message(db_serv:user_id(), db_serv:message_id()) -> ok | {error, access_denied}.

delete_message(UserId, MessageId) ->
    case dets:lookup(?MESSAGE_DB, MessageId) of
        [#message{author = UserId}] ->
            %% FIXME: Delete blobs and purge database
            ok = dets:delete(?MESSAGE_DB, MessageId);
        _ ->
            {error, access_denied}
    end.

%%
%% Utilities
%%

sort(Messages) ->
    lists:sort(
      fun(MessageA, MessageB) ->
              MessageA#message.created > MessageB#message.created
      end, Messages).
