% -*- fill-column: 100; -*-

-module(db_subscription_db).
-export([open/0, dump/0, close/0, subscribe/2, unsubscribe/1, inform_subscribers/1]).
-export_type([subscription_id/0]).

-define(SUBSCRIPTION_DB, subscription_db).

-type subscription_id() :: reference().
-type monitor_ref() :: reference().

-record(subscription, {
                       id :: subscription_id() | '_',
                       subscriber :: pid() | '_',
                       monitor_ref :: monitor_ref(),
                       post_ids :: [db:post_id()] | '_'
                      }).

%%
%% Exported: open
%%

-spec open() -> ok.

open() ->
    ?SUBSCRIPTION_DB =
        ets:new(?SUBSCRIPTION_DB, [{keypos, #subscription.id}, named_table, public]),
    ok.

%%
%% Exported: dump
%%

-spec dump() -> [{dets:tab_name(), [#subscription{}]}].

dump() ->
    [{?SUBSCRIPTION_DB, ets:tab2list(?SUBSCRIPTION_DB)}].

%%
%% Exported: close
%%

-spec close() -> ok.

close() ->
    true = ets:delete(?SUBSCRIPTION_DB),
    ok.

%%
%% Exported: subscribe
%%

-spec subscribe(pid(), [db:post_id()]) -> subscription_id().

subscribe(Subscriber, PostIds) ->
    SubscriptionId = make_ref(),
    true = ets:insert(?SUBSCRIPTION_DB, #subscription{id = SubscriptionId,
                                                      subscriber = Subscriber,
                                                      monitor_ref = monitor(process, Subscriber),
                                                      post_ids = PostIds}),
    SubscriptionId.

%%
%% Exported: unsubscribe
%%

-spec unsubscribe({monitor, monitor_ref()} | reference()) -> ok.

unsubscribe({monitor, MonitorRef}) ->
    true = ets:match_delete(?SUBSCRIPTION_DB, #subscription{monitor_ref = MonitorRef, _ = '_'}),
    ok;
unsubscribe(SubscriptionId) ->
    true = ets:delete(?SUBSCRIPTION_DB, SubscriptionId),
    ok.

%%
%% Exported: inform_subscribers
%%

-spec inform_subscribers(db:post_id()) -> ok.

inform_subscribers(PostId) ->
    ets:foldl(fun(#subscription{id = SubscriptionId,
                                subscriber = Subscriber,
                                monitor_ref = MonitorRef,
                                post_ids = PostIds}, Acc) ->
                      case lists:member(PostId, PostIds) of
                          true ->
                              Subscriber ! {subscription_change, SubscriptionId, PostId},
                              true = demonitor(MonitorRef),
                              true = ets:delete(?SUBSCRIPTION_DB, SubscriptionId),
                              Acc;
                          false ->
                              Acc
                      end
              end, ok, ?SUBSCRIPTION_DB).
