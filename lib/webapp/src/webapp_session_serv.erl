% -*- fill-column: 100; -*-

-module(webapp_session_serv).
-export([start_link/0, stop/0, subscription_started/2, subscription_ended/1, stop_subscriptions/1]).
-export([message_handler/1]).

-include_lib("apptools/include/serv.hrl").

-record(state, {subscriptions = #{} ::
                  #{db_subscription_db:subscription_id() =>
                        #{session_id => db_user_serv:session_id(),
                          socket_acceptor => pid()}},
                parent :: pid()}).

-include_lib("apptools/include/log.hrl").

%%
%% Exported: start_link
%%

-spec start_link() -> serv:spawn_server_result().

start_link() ->
    ?spawn_server(fun init/1, fun ?MODULE:message_handler/1, #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: subscription_started
%%

-spec subscription_started(db_user_serv:session_id(), db_subscription_db:subscription_id()) -> ok.

subscription_started(SessionId, SubscriptionId) ->
    serv:call(?MODULE, {subscription_started, SessionId, SubscriptionId, self()}).

%%
%% Exported: subscription_ended
%%

-spec subscription_ended(db_subscription_db:subscription_id()) -> ok.

subscription_ended(SubscriptionId) ->
    serv:call(?MODULE, {subscription_ended, SubscriptionId}).

%%
%% Exported: stop_subscriptions
%%

-spec stop_subscriptions(db_user_serv:session_id()) -> ok.

stop_subscriptions(SessionId) ->
    serv:call(?MODULE, {stop_subscriptions, SessionId}).

%%
%% Server
%%

init(Parent) ->
    ?log_info("Subscription manager started"),
    {ok, #state{parent = Parent}}.

message_handler(S) ->
    receive
        {call, From, stop = Call} ->
            ?log_info("Call: ~p", [Call]),
            {reply, From, ok};
        {call, From, {subscription_started, SessionId, SubscriptionId, SocketAcceptor}} ->
            ?log_info("Subscription ~w:~p has been started", [SessionId, SubscriptionId]),
            Subscriptions = S#state.subscriptions,
            UpdatedState =
                S#state{subscriptions =
                            Subscriptions#{SubscriptionId => #{session_id => SessionId,
                                                               socket_acceptor => SocketAcceptor}}},
            {reply, From, ok, UpdatedState};
        {call, From, {subscription_ended, SubscriptionId}} ->
            ?log_info("Subscription ~w has been ended", [SubscriptionId]),
            ok = db_serv:unsubscribe_on_changes(SubscriptionId),
            UpdatedState =
                S#state{subscriptions = maps:remove(SubscriptionId, S#state.subscriptions)},
            {reply, From, ok, UpdatedState};
        {call, From, {stop_subscriptions, SessionId}} ->
            ?log_info("Stopping subscriptions for session ~w", [SessionId]),
            UpdatedSubscriptions =
                maps:fold(
                  fun(SubscriptionId, #{session_id := CurrentSessionId,
                                        socket_acceptor := SocketAcceptor}, Acc)
                        when CurrentSessionId == SessionId ->
                          ok = db_serv:unsubscribe_on_changes(SubscriptionId),
                          SocketAcceptor ! stop_subscription,
                          maps:remove(SubscriptionId, Acc);
                     (_, _, Acc) ->
                          Acc
                  end, S#state.subscriptions, S#state.subscriptions),
            {reply, From, ok, S#state{subscriptions = UpdatedSubscriptions}};
        {'EXIT', Pid, Reason} when S#state.parent == Pid ->
            exit(Reason);
        {system, From, Request} ->
            ?log_info("System: ~p", [Request]),
            {system, From, Request};
        UnknownMessage ->
            ?log_error("Unknown message: ~p", [UnknownMessage]),
            noreply
    end.
