% -*- fill-column: 100; -*-

-module(db_message_db).
-export([open/0, dump/0, sync/0, close/0, create_message/4, read_top_messages/1,
         read_reply_messages/2, delete_message/2]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include("../include/db.hrl").
-include("db_message_db.hrl").

%%
%% Exported: open
%%

-spec open() -> ok | {error, file:posix()}.

open() ->
    maybe
        {ok, _} ?= db:open_disk(?MESSAGE_DB, ?MESSAGE_FILE_PATH, #message.id),
        {ok, _} ?= db:open_disk_index(?MESSAGE_INDEX_DB, ?MESSAGE_INDEX_FILE_PATH),
        {ok, _} ?= db:open_disk(?MESSAGE_RECIPIENT_DB, ?MESSAGE_RECIPIENT_FILE_PATH,
                                #message_recipient.message_id),
        {ok, _} ?= db:open_disk_index(?MESSAGE_RECIPIENT_INDEX_DB,
                                      ?MESSAGE_RECIPIENT_INDEX_FILE_PATH),
        {ok, _} ?= db:open_disk(?MESSAGE_ATTACHMENT_DB, ?MESSAGE_ATTACHMENT_FILE_PATH,
                                #message_attachment.id),
        {ok, _} ?= db:open_disk_index(?MESSAGE_ATTACHMENT_INDEX_DB,
                                      ?MESSAGE_ATTACHMENT_INDEX_FILE_PATH),
        ok
    else
        Error ->
            Error
    end.

%%
%% Exported: dump
%%

-spec dump() -> [{dets:tab_name(), [term()]}].

dump() ->
    [{?MESSAGE_DB, db:dump_disk(?MESSAGE_DB)},
     {?MESSAGE_INDEX_DB, db:dump_disk_index(?MESSAGE_INDEX_DB)},
     {?MESSAGE_RECIPIENT_DB, db:dump_disk(?MESSAGE_RECIPIENT_DB)},
     {?MESSAGE_RECIPIENT_INDEX_DB, db:dump_disk_index(?MESSAGE_RECIPIENT_INDEX_DB)},
     {?MESSAGE_ATTACHMENT_DB, db:dump_disk(?MESSAGE_ATTACHMENT_DB)},
     {?MESSAGE_ATTACHMENT_INDEX_DB, db:dump_disk_index(?MESSAGE_ATTACHMENT_INDEX_DB)}].

%%
%% Exported: sync
%%

-spec sync() -> ok | {error, term()}.

sync() ->
    maybe
        ok ?= db:sync_disk_index(?MESSAGE_ATTACHMENT_INDEX_DB),
        ok ?= db:sync_disk(?MESSAGE_ATTACHMENT_DB),
        ok ?= db:sync_disk_index(?MESSAGE_RECIPIENT_INDEX_DB),
        ok ?= db:sync_disk(?MESSAGE_RECIPIENT_DB),
        ok ?= db:sync_disk_index(?MESSAGE_INDEX_DB),
        ok ?= db:sync_disk(?MESSAGE_DB)
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
        ok ?= db:close_disk_index(?MESSAGE_ATTACHMENT_INDEX_DB),
        ok ?= db:close_disk(?MESSAGE_ATTACHMENT_DB),
        ok ?= db:close_disk_index(?MESSAGE_RECIPIENT_INDEX_DB),
        ok ?= db:close_disk(?MESSAGE_RECIPIENT_DB),
        ok ?= db:close_disk_index(?MESSAGE_INDEX_DB),
        ok ?= db:close_disk(?MESSAGE_DB)
    else
        Error ->
            Error
    end.

%%
%% Exported: create_message
%%

-spec create_message(db_serv:user_id(), #message{},
                     [{db_serv:user_id(), main:filename()}],
                     [[{db_serv:user_id(), main:filename()}]]) ->
          {ok, #message{}} | {error, term()}.

create_message(UserId, Message, BodyBlobs, AttachmentBlobs) ->
    MessageId = db_meta_db:read_next_message_id(),
    case handle_blobs(MessageId, BodyBlobs, AttachmentBlobs) of
        ok ->
            UpdatedMessage =
                Message#message{
                  id = MessageId,
                  created = db:seconds_since_epoch()
                 },
            ok = db:insert_disk(?MESSAGE_DB, UpdatedMessage),
            ok = db:insert_disk_index(?MESSAGE_RECIPIENT_INDEX_DB, UserId, MessageId),
            {ok, UpdatedMessage};
        Error ->
            Error
    end.

handle_blobs(MessageId, BodyBlobs, AttachmentBlobs) ->
    maybe
        %% Note: Disk layout is described in db.hrl
        MessageBlobPath = filename:join([?BESPOKE_MESSAGE_PATH, ?i2b(MessageId)]),
        ok ?= make_dir(MessageBlobPath),
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
    ?log_info("Renaming ~s to ~s", [CurrentBlobPath, NewBlobPath]),
    ok = file:rename(CurrentBlobPath, NewBlobPath),
    ok = db:insert_disk(?MESSAGE_RECIPIENT_DB,
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
    ?log_info("Renaming ~s to ~s", [CurrentBlobPath, NewBlobPath]),
    ok = file:rename(CurrentBlobPath, NewBlobPath),
    ok = db:insert_disk(?MESSAGE_ATTACHMENT_DB,
                        #message_attachment{id = AttachmentId, message_id = MessageId}),
    handle_attachment_blobs(MessageId, MessageBlobPath, Rest).

%%
%% Exported: read_top_messages
%%

-spec read_top_messages(db_serv:user_id()) -> {ok, [#message{}]}.

read_top_messages(UserId) ->
    MessageIds = db:lookup_disk_index(?MESSAGE_RECIPIENT_INDEX_DB, UserId),
    Messages =
        lists:foldl(
          fun(MessageId, Acc) ->
                  case db:lookup_disk(?MESSAGE_DB, MessageId) of
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
    RecipientMessageIds = db:lookup_disk_index(?MESSAGE_RECIPIENT_INDEX_DB, UserId),
    case lists:member(TopMessageId, RecipientMessageIds) of
        true ->
            MessageIds = db:lookup_disk_index(?MESSAGE_INDEX_DB, TopMessageId),
            Messages =
                lists:foldl(fun(MessageId, Acc) ->
                                    [Message] = db:lookup_disk(?MESSAGE_DB, MessageId),
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
    case db:lookup_disk(?MESSAGE_DB, MessageId) of
        [#message{author = UserId}] ->
            %% FIXME: Delete blobs and purge database
            ok = db:delete_disk(?MESSAGE_DB, MessageId);
        [] ->
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

make_dir(DirPath) ->
    case filelib:is_dir(DirPath) of
        true ->
            ok;
        false ->
            file:make_dir(DirPath)
    end.
