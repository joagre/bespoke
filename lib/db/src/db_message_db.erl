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

create_message(#message{top_message_id = TopMessageId, author = Author} = Message,
               BodyBlobs, AttachmentBlobs) ->
    RecipientMessageIds =
        case TopMessageId of
            %% Is a top message!
            not_set ->
                ignore;
            %% Is a reply message!
            _ ->
                db:lookup_disk_index(?MESSAGE_RECIPIENT_INDEX_DB, TopMessageId)
        end,
    case RecipientMessageIds == ignore orelse lists:member(Author, RecipientMessageIds) of
        true ->
            MessageId = db_meta_db:read_next_message_id(),
            UpdatedMessage =
                Message#message{
                  id = MessageId,
                  created = db:seconds_since_epoch()
                 },
            %% Update MESSAGE_DB
            ok = db:insert_disk(?MESSAGE_DB, UpdatedMessage),
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
        ok ?= handle_body_blobs(Message, MessageBlobPath, BodyBlobs),
        ok ?= handle_attachment_blobs(Message, MessageBlobPath, AttachmentBlobs)
    else
        Error ->
            Error
    end.

handle_body_blobs(_Message, _MessageBlobPath, []) ->
    ok;
handle_body_blobs(#message{id = MessageId, top_message_id = TopMessageId} = Message,
                  MessageBlobPath, [{UserId, BlobFilename}|Rest]) ->
    CurrentBlobPath = filename:join([?BESPOKE_TMP_PATH, BlobFilename]),
    NewBlobPath = filename:join([MessageBlobPath, ?i2b(UserId)]),
    ?log_info("Renaming ~s to ~s", [CurrentBlobPath, NewBlobPath]),
    case file:rename(CurrentBlobPath, NewBlobPath) of
        ok ->
            case TopMessageId of
                %% Is a top message!
                not_set ->
                    %% Update MESSAGE_TOP_INDEX_DB and MESSAGE_RECIPIENT_INDEX_DB
                    ok = db:insert_disk_index(?MESSAGE_TOP_INDEX_DB, UserId, MessageId),
                    ok = db:insert_disk_index(?MESSAGE_RECIPIENT_INDEX_DB, MessageId, UserId);
                %% Is a reply message!
                TopMessageId ->
                    %% Update MESSAGE_REPLY_INDEX_DB and MESSAGE_RECIPIENT_INDEX_DB
                    ok = db:insert_disk_index(?MESSAGE_REPLY_INDEX_DB, TopMessageId, MessageId),
                    ok = db:insert_disk_index(?MESSAGE_RECIPIENT_INDEX_DB, TopMessageId, UserId)
            end,
            handle_body_blobs(Message, MessageBlobPath, Rest);
        {error, Reason} ->
            {error, Reason}
    end.

handle_attachment_blobs(_Message, _MessageBlobPath, []) ->
    ok;
handle_attachment_blobs(Message, MessageBlobPath, [AttachmentBlobs|Rest]) ->
    AttachmentId = db_meta_db:read_next_message_attachment_id(),
    case handle_attachment_blobs(AttachmentId, Message, MessageBlobPath, AttachmentBlobs) of
        ok ->
            handle_attachment_blobs(Message, MessageBlobPath, Rest);
        {error, Reason} ->
            {error, Reason}
    end.

handle_attachment_blobs(_AttachmentId, _Message, _MessageBlobPath, []) ->
    ok;
handle_attachment_blobs(AttachmentId, #message{id = MessageId} = Message, MessageBlobPath,
                        [{UserId, BlobFilename}|Rest]) ->
    CurrentBlobPath = filename:join([?BESPOKE_TMP_PATH, BlobFilename]),
    NewBlobPath = filename:join([MessageBlobPath, io_lib:format("~w-~w", [UserId, AttachmentId])]),
    ?log_info("Renaming ~s to ~s", [CurrentBlobPath, NewBlobPath]),
    case file:rename(CurrentBlobPath, NewBlobPath) of
        ok ->
            %% Update MESSAGE_ATTACHMENT_INDEX_DB
            ok = db:insert_disk_index(?MESSAGE_ATTACHMENT_INDEX_DB, MessageId, AttachmentId),
            handle_attachment_blobs(AttachmentId, Message, MessageBlobPath, Rest);
        {error, Reason} ->
            {error, Reason}
    end.

%%
%% Exported: read_top_messages
%%

-spec read_top_messages(db_serv:user_id()) ->
          {ok, [{{#message{}, [db_serv:message_attachment_id()]}}]}.

read_top_messages(UserId) ->
    MessageIds = db:lookup_disk_index(?MESSAGE_TOP_INDEX_DB, UserId),
    {ok, read_messages(MessageIds)}.

read_messages(MessageIds) ->
    Messages = sort_messages(lookup_messages(MessageIds)),
    lists:map(fun(#message{id = MessageId} = Message) ->
                      AttachmentIds = db:lookup_disk_index(?MESSAGE_ATTACHMENT_INDEX_DB, MessageId),
                      {Message, AttachmentIds}
              end, Messages).

lookup_messages([]) ->
    [];
lookup_messages([MessageId|Rest]) ->
    [Message] = db:lookup_disk(?MESSAGE_DB, MessageId),
    [Message|lookup_messages(Rest)].

%%
%% Exported: read_reply_messages
%%

-spec read_reply_messages(db_serv:user_id(), db_serv:message_id()) ->
          {ok, [{{#message{}, [db_serv:message_attachment_id()]}}]} |
          {error, access_denied}.

read_reply_messages(UserId, TopMessageId) ->
    RecipientMessageIds = db:lookup_disk_index(?MESSAGE_RECIPIENT_INDEX_DB, UserId),
    case lists:member(TopMessageId, RecipientMessageIds) of
        true ->
            MessageIds = db:lookup_disk_index(?MESSAGE_REPLY_INDEX_DB, TopMessageId),
            {ok, read_messages(MessageIds)};
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

sort_messages(Messages) ->
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
