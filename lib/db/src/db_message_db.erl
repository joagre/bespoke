% -*- fill-column: 100; -*-

-module(db_message_db).
-export([open/0, dump/0, sync/0, close/0, create_message/3, read_top_messages/1,
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
        {ok, _} ?= db:open_disk_index(?MESSAGE_TOP_INDEX_DB, ?MESSAGE_TOP_INDEX_FILE_PATH),
        {ok, _} ?= db:open_disk_index(?MESSAGE_REPLY_INDEX_DB, ?MESSAGE_REPLY_INDEX_FILE_PATH),
        {ok, _} ?=
            db:open_disk_index(?MESSAGE_RECIPIENT_INDEX_DB, ?MESSAGE_RECIPIENT_INDEX_FILE_PATH),
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
     {?MESSAGE_TOP_INDEX_DB, db:dump_disk_index(?MESSAGE_TOP_INDEX_DB)},
     {?MESSAGE_REPLY_INDEX_DB, db:dump_disk_index(?MESSAGE_REPLY_INDEX_DB)},
     {?MESSAGE_RECIPIENT_INDEX_DB, db:dump_disk_index(?MESSAGE_RECIPIENT_INDEX_DB)},
     {?MESSAGE_ATTACHMENT_INDEX_DB, db:dump_disk_index(?MESSAGE_ATTACHMENT_INDEX_DB)}].

%%
%% Exported: sync
%%

-spec sync() -> ok | {error, term()}.

sync() ->
    maybe
        ok ?= db:sync_disk(?MESSAGE_DB),
        ok ?= db:sync_disk_index(?MESSAGE_TOP_INDEX_DB),
        ok ?= db:sync_disk_index(?MESSAGE_REPLY_INDEX_DB),
        ok ?= db:sync_disk_index(?MESSAGE_RECIPIENT_INDEX_DB),
        ok ?= db:sync_disk_index(?MESSAGE_ATTACHMENT_INDEX_DB)
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
        ok ?= db:close_disk(?MESSAGE_DB),
        ok ?= db:close_disk_index(?MESSAGE_TOP_INDEX_DB),
        ok ?= db:close_disk_index(?MESSAGE_REPLY_INDEX_DB),
        ok ?= db:close_disk_index(?MESSAGE_RECIPIENT_INDEX_DB),
        ok ?= db:close_disk_index(?MESSAGE_ATTACHMENT_INDEX_DB)
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
    UpdatedMessage =
        Message#message{
          id = MessageId,
          created = db:seconds_since_epoch()
         },
    case create_blobs(UpdatedMessage, BodyBlobs, AttachmentBlobs) of
        ok ->
            %% Update MESSAGE_DB
            ok = db:insert_disk(?MESSAGE_DB, UpdatedMessage),
            {ok, UpdatedMessage};
        Error ->
            Error
    end.

create_blobs(#message{id = MessageId} = Message, BodyBlobs, AttachmentBlobs) ->
    maybe
        %% Note: Disk layout is described in db.hrl
        MessageBlobPath = filename:join([?BESPOKE_MESSAGE_PATH, ?i2b(MessageId)]),
        ok ?= make_dir(MessageBlobPath),
        ok ?= handle_body_blobs(Message, MessageBlobPath, BodyBlobs),
        ok ?= handle_attachment_blobs(Message, MessageBlobPath, AttachmentBlobs)
    else
        Error ->
            Error
    end.

handle_body_blobs(_Message, _MessageBlobPath, []) ->
    ok;
handle_body_blobs(#message{id = MessageId} = Message, MessageBlobPath,
                  [{UserId, BlobFilename}|Rest]) ->
    CurrentBlobPath = filename:join([?BESPOKE_TMP_PATH, BlobFilename]),
    NewBlobPath = filename:join([MessageBlobPath, ?i2b(UserId)]),
    ?log_info("Renaming ~s to ~s", [CurrentBlobPath, NewBlobPath]),
    ok = file:rename(CurrentBlobPath, NewBlobPath),
    %% Update MESSAGE_TOP_INDEX_DB and MESSAGE_REPLY_INDEX_DB
    case Message#message.top_message_id of
        not_set ->
            ok = db:insert_disk_index(?MESSAGE_TOP_INDEX_DB, UserId, MessageId);
        TopMessageId ->
            ok = db:insert_disk_index(?MESSAGE_REPLY_INDEX_DB, TopMessageId, MessageId)
    end,
    %% Update MESSAGE_RECPIENT_INDEX_DB
    ok = db:insert_disk_index(?MESSAGE_RECIPIENT_INDEX_DB, MessageId, UserId),
    handle_body_blobs(Message, MessageBlobPath, Rest).

handle_attachment_blobs(_Message, _MessageBlobPath, []) ->
    ok;
handle_attachment_blobs(Message, MessageBlobPath, [AttachmentBlobs|Rest])
  when is_list(AttachmentBlobs) ->
    ok = handle_attachment_blobs(Message, MessageBlobPath, AttachmentBlobs),
    handle_attachment_blobs(Message, MessageBlobPath, Rest);
handle_attachment_blobs(#message{id = MessageId} = Message, MessageBlobPath,
                        [{UserId, BlobFilename}|Rest]) ->
    CurrentBlobPath = filename:join([?BESPOKE_TMP_PATH, BlobFilename]),
    AttachmentId = db_meta_db:read_next_message_attachment_id(),
    NewBlobPath = filename:join([MessageBlobPath, io_lib:format("~w-~w", [UserId, AttachmentId])]),
    ?log_info("Renaming ~s to ~s", [CurrentBlobPath, NewBlobPath]),
    ok = file:rename(CurrentBlobPath, NewBlobPath),
    %% Update MESSAGE_ATTACHMENT_INDEX_DB
    ok = db:insert_disk_index(?MESSAGE_ATTACHMENT_INDEX_DB, MessageId, AttachmentId),
    handle_attachment_blobs(Message, MessageBlobPath, Rest).

%%
%% Exported: read_top_messages
%%

-spec read_top_messages(db_serv:user_id()) -> {ok, [#message{}]}.

read_top_messages(UserId) ->
    MessageIds = db:lookup_disk_index(?MESSAGE_TOP_INDEX_DB, UserId),
    {ok, sort(get_messages(MessageIds))}.

%%
%% Exported: read_reply_messages
%%

-spec read_reply_messages(db_serv:user_id(), db_serv:message_id()) ->
          {ok, [#message{}]} | {error, access_denied}.

read_reply_messages(UserId, TopMessageId) ->
    RecipientMessageIds = db:lookup_disk_index(?MESSAGE_RECIPIENT_INDEX_DB, UserId),
    case lists:member(TopMessageId, RecipientMessageIds) of
        true ->
            MessageIds = db:lookup_disk_index(?MESSAGE_REPLY_INDEX_DB, TopMessageId),
            {ok, sort(get_messages(MessageIds))};
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
            ok = delete_blobs(MessageId),
            %% Update MESSAGE_DB
            ok = db:delete_disk(?MESSAGE_DB, MessageId),
            %% Update MESSAGE_TOP_INDEX_DB
            ok = db:delete_disk_index(?MESSAGE_TOP_INDEX_DB, UserId),
            %% Update MESSAGE_REPLY_INDEX_DB
            ok = db:delete_disk_index(?MESSAGE_REPLY_INDEX_DB, MessageId);
        [] ->
            {error, access_denied}
    end.

delete_blobs(_MessageId) ->
    %% FIXME
    ok.

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

get_messages([]) ->
    [];
get_messages([MessageId|Rest]) ->
    [Message] = db:lookup_disk(?MESSAGE_DB, MessageId),
    [Message|get_messages(Rest)].
