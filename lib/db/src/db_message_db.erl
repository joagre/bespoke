% -*- fill-column: 100; -*-

-module(db_message_db).
-export([open/0, sync/0, close/0, get_top_level_messages/1, get_reply_messages/2]).

-include("../include/db.hrl").
-include("db_message_db.hrl").

%%
%% Exported: open
%%

-spec open() -> ok | {error, term()}.

open() ->
    maybe
        {ok, _} = db:open_disk_db(?MESSAGE_DB, ?MESSAGE_FILENAME, #message.id),
        {ok, _} = db:open_disk_index_db(?MESSAGE_INDEX_DB, ?MESSAGE_INDEX_FILENAME),
        {ok, _} = db:open_disk_db(?MESSAGE_RECIPIENT_DB, ?MESSAGE_RECIPIENT_FILENAME,
                                  #message_recipient.message_id),
        {ok, _} = db:open_disk_index_db(?MESSAGE_RECIPIENT_INDEX_DB,
                                        ?MESSAGE_RECIPIENT_INDEX_FILENAME),
        {ok, _} = db:open_disk_db(?MESSAGE_ATTACHMENT_DB, ?MESSAGE_ATTACHMENT_FILENAME,
                                  #message_attachment.id),
        {ok, _} = db:open_disk_index_db(?MESSAGE_ATTACHMENT_INDEX_DB,
                                        ?MESSAGE_ATTACHMENT_INDEX_FILENAME),
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
        ok = db:sync_disk_index_db(?MESSAGE_ATTACHMENT_INDEX_DB),
        ok = db:sync_disk_db(?MESSAGE_ATTACHMENT_DB),
        ok = db:sync_disk_index_db(?MESSAGE_RECIPIENT_INDEX_DB),
        ok = db:sync_disk_db(?MESSAGE_RECIPIENT_DB),
        ok = db:sync_disk_index_db(?MESSAGE_INDEX_DB),
        ok = db:sync_disk_db(?MESSAGE_DB)
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
        ok = db:close_disk_index_db(?MESSAGE_ATTACHMENT_INDEX_DB),
        ok = db:close_disk_db(?MESSAGE_ATTACHMENT_DB),
        ok = db:close_disk_index_db(?MESSAGE_RECIPIENT_INDEX_DB),
        ok = db:close_disk_db(?MESSAGE_RECIPIENT_DB),
        ok = db:close_disk_index_db(?MESSAGE_INDEX_DB),
        ok = db:close_disk_db(?MESSAGE_DB)
    else
        Error ->
            Error
    end.

%%
%% Exported: get_top_level_messages
%%

-spec get_top_level_messages(db_serv:user_id()) -> [#message{}].

get_top_level_messages(UserId) ->
    MessageIds = dets:lookup(?MESSAGE_RECIPIENT_INDEX_DB, UserId),
    TopLevelMessages =
        lists:foldl(
          fun(MessageId, Acc) ->
                  case dets:lookup(?MESSAGE_DB, MessageId) of
                      [#message{parent_message_id = not_set} = Message] ->
                          [Message|Acc];
                      _ ->
                          Acc
                  end
          end, [], MessageIds),
    sort(TopLevelMessages).

sort(Messages) ->
    lists:sort(
      fun(MessageA, MessageB) ->
              MessageA#message.created > MessageB#message.created
      end, Messages).

%%
%% Exported: get_reply_messages
%%

-spec get_reply_messages(db_serv:user_id(), db_serv:message_id()) -> [#message{}].

get_reply_messages(UserId, TopLevelMessageId) ->
    RecipientMessageIds = dets:lookup(?MESSAGE_RECIPIENT_DB, UserId),
    case lists:member(TopLevelMessageId, RecipientMessageIds) of
        true ->
            ReplyMessageIds = dets:lookup(?MESSAGE_INDEX_DB, TopLevelMessageId),
            ReplyMessages =
                lists:foldl(fun(MessageId, Acc) ->
                                    [Message] = dets:lookup(?MESSAGE_DB, MessageId),
                                    [Message|Acc]
                            end, [], ReplyMessageIds),
            sort(ReplyMessages);
        false ->
            []
    end.
