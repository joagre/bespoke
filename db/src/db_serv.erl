-module(db_serv).
-export([start_link/0, stop/0]).
-export([list_root_messages/0,
         lookup_messages/1, lookup_messages/2,
         insert_message/1,
         delete_message/1]).
-export([sync/0]).
-export([message_handler/1]).
-export_type([message_id/0, title/0, body/0, author/0, seconds_since_epoch/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/serv.hrl").
-include("db.hrl").

-type message_id() :: binary().
-type title() :: binary().
-type body() :: binary().
-type author() :: binary().
-type seconds_since_epoch() :: integer().

-record(state, {
                parent :: pid(),
                next_message_id = 0 :: integer()
               }).

%%
%% Exported: start_link
%%

-spec start_link() -> serv:spawn_server_result().

start_link() ->
    ?spawn_server(fun init/1, fun ?MODULE:message_handler/1,
                  #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: list_root_messages
%%

-spec list_root_messages() -> [#message{}].

list_root_messages() ->
    serv:call(?MODULE, list_root_messages).

%%
%% Exported: lookup_messages
%%

-spec lookup_messages([message_id()], flat | recursive) -> [#message{}].

lookup_messages(MessageIds) ->
    lookup_messages(MessageIds, flat).

lookup_messages(MessageIds, Mode) ->
    serv:call(?MODULE, {lookup_messages, MessageIds, Mode}).

%%
%% Exported: insert_message
%%

-spec insert_message(#message{}) -> {ok, #message{}} | {error, invalid_message}.

insert_message(Message) ->
    serv:call(?MODULE, {insert_message, Message}).

%%
%% delete_message
%%

-spec delete_message(message_id()) -> ok | {error, not_found}.

delete_message(MessageId) ->
    serv:call(?MODULE, {delete_message, MessageId}).

%%
%% Exported: sync
%%

-spec sync() -> ok.

sync() ->
    serv:call(?MODULE, sync).

%%
%% Server
%%

init(Parent) ->
    {ok, messages} =
        dets:open_file(
          messages,
          [{file, filename:join(code:priv_dir(db), "messages.db")},
           {keypos, #message.id}]),
    {ok, meta} =
        dets:open_file(
          meta,
          [{file, filename:join(code:priv_dir(db), "meta.db")},
           {keypos, #meta.type}]),
    case dets:lookup(meta, basic) of
        [] ->
            ok = dets:insert(meta, #meta{type = basic,
                                         next_message_id = 0}),
            NextMessageId = 0;
        [#meta{next_message_id = NextMessageId}] ->
            ok
    end,
    ?log_info("Database server has been started"),
    {ok, #state{parent = Parent, next_message_id = NextMessageId}}.

message_handler(S) ->
    receive
        {call, From, stop = Call} ->
            ?log_debug(#{call => Call}),
            ok = dets:close(messages),
            {reply, From, ok};
        {call, From, list_root_messages = Call} ->
            ?log_debug(#{call => Call}),
            RootMessages =
                dets:match_object(
                  messages, #message{root_message_id = not_set, _ = '_'}),
            SortedRootMessages =
                lists:sort(
                  fun(MessageA, MessageB) ->
                          MessageA#message.created =< MessageB#message.created
                  end, RootMessages),
            {reply, From, SortedRootMessages};
        {call, From, {lookup_messages, MessageIds, Mode} = Call} ->
            ?log_debug(#{call => Call}),
            {reply, From, do_lookup_messages(MessageIds, Mode)};
        {call, From, {insert_message, Message} = Call} ->
            ?log_debug(#{call => Call}),
            case do_insert_message(S#state.next_message_id, Message) of
                {ok, NextUpcomingMessageId, InsertedMessage} ->
                    {reply, From, {ok, InsertedMessage},
                     S#state{next_message_id = NextUpcomingMessageId}};
                {error, Reason} ->
                    {reply, From, {error, Reason}}
            end;
        {call, From, {delete_message, MessageId} = Call} ->
            ?log_debug(#{call => Call}),
            case dets:lookup(messages, MessageId) of
                [#message{parent_message_id = ParentMessageId}]
                  when ParentMessageId /= not_set ->
                    [ParentMessage] = dets:lookup(messages, ParentMessageId),
                    ok = dets:insert(
                           messages,
                           ParentMessage#message{
                             replies = lists:delete(
                                         MessageId,
                                         ParentMessage#message.replies)}),
                    N = delete_all([MessageId]),
                    ok = update_parent_count(ParentMessage#message.id, -N),
                    {reply, From, ok};
                [_RootMessage] ->
                    _ = delete_all([MessageId]),
                    {reply, From, ok};
                [] ->
                    {reply, From, {error, not_found}}
            end;
        {call, From, sync = Call} ->
            ?log_debug(#{call => Call}),
            ok = dets:sync(messages),
            {reply, From, ok};
        {'EXIT', Pid, Reason} when S#state.parent == Pid ->
            exit(Reason);
        {system, From, Request} ->
            ?log_debug(#{system => {From, Request}}),
            {system, From, Request};
        UnknownMessage ->
            ?log_error(#{unknown_message => UnknownMessage}),
            noreply
    end.

%%
%% Insert message
%%

do_insert_message(NextMessageId, Message) ->
    case is_valid_insert_message(Message) of
        {true, ParentMessage, _RootMessage}  ->
            case Message#message.id of
                not_set ->
                    NextUpcomingMessageId = NextMessageId + 1,
                    UpdatedMeta =
                        #meta{type = basic,
                              next_message_id = NextUpcomingMessageId},
                    ok = dets:insert(meta, UpdatedMeta),
                    NewMessageId = ?i2b(NextMessageId);
                MessageId ->
                    NewMessageId = MessageId,
                    NextUpcomingMessageId = NextMessageId
            end,
            UpdatedMessage =
                Message#message{
                  id = NewMessageId,
                  created = seconds_since_epoch(Message#message.created)},
            ok = dets:insert(messages, UpdatedMessage),
            case ParentMessage of
                not_set ->
                    {ok, NextUpcomingMessageId, UpdatedMessage};
                #message{replies = Replies} ->
                    UpdatedParentMessage =
                        ParentMessage#message{
                          replies = Replies ++ [NewMessageId]},
                    ok = dets:insert(messages, UpdatedParentMessage),
                    ok = update_parent_count(ParentMessage#message.id, 1),
                    {ok, NextUpcomingMessageId, UpdatedMessage}
            end;
        false ->
            {error, invalid_message}
    end.

is_valid_insert_message(#message{id = not_set} = Message) ->
    check_insert_message(Message);
is_valid_insert_message(Message) ->
    case dets:lookup(messages, Message#message.id) of
        [] ->
            check_insert_message(Message);
        _ ->
            false
    end.

check_insert_message(#message{title = Title,
                              parent_message_id = not_set,
                              root_message_id = not_set,
                              created = Created})
  when Title /= not_set andalso
       (Created == not_set orelse is_integer(Created)) ->
    {true, not_set, not_set};
check_insert_message(#message{title = not_set,
                              parent_message_id = ParentMessageId,
                              root_message_id = RootMessageId,
                              created = Created})
  when ParentMessageId /= not_set andalso
       RootMessageId /= not_set andalso
       (Created == not_set orelse is_integer(Created)) ->
    case dets:lookup(messages, ParentMessageId) of
        [ParentMessage] ->
            case dets:lookup(messages, RootMessageId) of
                [RootMessage] ->
                    {true, ParentMessage, RootMessage};
                [] ->
                    false
            end;
        [] ->
            false
    end;
check_insert_message(_) ->
    false.

update_parent_count(not_set, _N) ->
    ok;
update_parent_count(MessageId, N) ->
    [Message] = dets:lookup(messages, MessageId),
    ok = dets:insert(messages,
                     Message#message{
                       reply_count = Message#message.reply_count + N}),
    update_parent_count(Message#message.parent_message_id, N).

%%
%% Lookup messages
%%

do_lookup_messages(MessageIds, Mode) ->
    Messages =
        lists:foldr(
          fun(MessageId, Acc) ->
                  case dets:lookup(messages, MessageId) of
                      [Message] when Mode == flat ->
                          [Message|Acc];
                      [Message] when Mode == recursive ->
                          Replies =
                              do_lookup_messages(Message#message.replies, Mode),
                          [Message|Acc] ++ Replies
                  end
          end, [], MessageIds),
    lists:sort(
      fun(MessageA, MessageB) ->
              MessageA#message.created =< MessageB#message.created
      end, Messages).

%%
%% Delete all messages (recursively)
%%

delete_all([]) ->
    0;
delete_all([MessageId|Rest]) ->
    [Message] = dets:lookup(messages, MessageId),
    ok = dets:delete(messages, MessageId),
    delete_all(Message#message.replies) + delete_all(Rest) + 1.

%%
%% Utilities
%%

seconds_since_epoch(not_set) ->
    os:system_time(second);
seconds_since_epoch(Seconds) ->
    Seconds.
