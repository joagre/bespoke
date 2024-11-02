-module(db_serv).

-export([start_link/0, stop/0]).
-export([list_root_messages/0, lookup_messages/1, insert_message/1]).
-export([sync/0]).
-export([message_handler/1]).

-export_type([message_id/0, title/0, body/0, author/0, seconds_from_epoch/0]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/shorthand.hrl").
-include_lib("apptools/include/serv.hrl").
-include("db.hrl").

-type message_id() :: integer().
-type title() :: string().
-type body() :: string().
-type author() :: string().
-type seconds_from_epoch() :: integer().

-record(state, {
                parent :: pid(),
                next_message_id = 0 :: message_id()
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

-spec lookup_messages([message_id()]) -> [#message{}].

lookup_messages(MessageIds) ->
    serv:call(?MODULE, {lookup_messages, MessageIds}).

%%
%% Exported: insert_message
%%

-spec insert_message(#message{}) -> {ok, #message{}} | {error, invalid_message}.

insert_message(Message) ->
    serv:call(?MODULE, {insert_message, Message}).

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
        dets:open_file(messages,
                       [{file, filename:join(code:priv_dir(db), "messages.db")},
                        {keypos, #message.id}]),
    ?log_info("Database server has been started"),
    {ok, #state{parent = Parent}}.

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
        {call, From, {lookup_messages, MessageIds} = Call} ->
            ?log_debug(#{call => Call}),
            Messages =
                lists:foldl(fun(MessageId, Acc) ->
                                    case dets:lookup(messages, MessageId) of
                                        [Message] ->
                                            [Message|Acc];
                                        [] ->
                                            Acc
                                    end
                            end, [], MessageIds),
            {reply, From, Messages};
        {call, From, {insert_message, Message} = Call} ->
            ?log_debug(#{call => Call}),
            case add_message(S#state.next_message_id, Message) of
                {ok, NextUpcomingMessageId, AddedMessage} ->
                    {reply, From, {ok, AddedMessage},
                     S#state{next_message_id = NextUpcomingMessageId}};
                {error, Reason} ->
                    {reply, From, {error, Reason}}
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

add_message(NextMessageId, Message) ->
    case is_valid_message(Message) of
        {true, ReplyMessage, _RootMessage}  ->
            NewMessage = Message#message{id = NextMessageId,
                                         created = seconds_from_epoch()},
            ok = dets:insert(messages, NewMessage),
            case ReplyMessage of
                not_set ->
                    {ok, NextMessageId + 1, NewMessage};
                #message{replies = Replies} ->
                    UpdatedReplyMessage =
                        ReplyMessage#message{
                          replies = Replies ++ [NextMessageId]},
                    ok = dets:insert(messages, UpdatedReplyMessage),
                    ok = update_reply_count(ReplyMessage#message.id),
                    {ok, NextMessageId + 1, NewMessage}
            end;
        false ->
            {error, invalid_message}
    end.

is_valid_message(#message{id = not_set,
                          title = Title,
                          reply_message_id = not_set,
                          root_message_id = not_set,
                          created = not_set}) when Title /= not_set ->
    {true, not_set, not_set};
is_valid_message(#message{id = not_set,
                          title = not_set,
                          reply_message_id = ReplyMessageId,
                          root_message_id = RootMessageId,
                          created = not_set})
  when ReplyMessageId /= not_set andalso
       RootMessageId /= not_set ->
    case dets:lookup(messages, ReplyMessageId) of
        [ReplyMessage] ->
            case dets:lookup(messages, RootMessageId) of
                [RootMessage] ->
                    {true, ReplyMessage, RootMessage};
                [] ->
                    false
            end;
        [] ->
            false
    end;
is_valid_message(_) ->
    false.

update_reply_count(not_set) ->
    ok;
update_reply_count(MessageId) ->
    [Message] = dets:lookup(messages, MessageId),
    ok = dets:insert(messages,
                     Message#message{
                       reply_count = Message#message.reply_count + 1}),
    update_reply_count(Message#message.reply_message_id).

%%
%% Utilities
%%

seconds_from_epoch() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).
