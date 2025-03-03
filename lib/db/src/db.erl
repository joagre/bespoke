% -*- fill-column: 100; -*-

-module(db).
-export([dets_dump/1, seconds_since_epoch/0, seconds_since_epoch/1]).
-export_type([seconds_since_epoch/0]).

-type seconds_since_epoch() :: integer().

%%
%% Exported: dets_dump
%%

dets_dump(Name) ->
    dets:foldl(fun(Entry, Acc) -> [Entry|Acc] end, [], Name).

%%
%% Exported: seconds_since_epoch
%%

-spec seconds_since_epoch() -> seconds_since_epoch().

seconds_since_epoch() ->
    os:system_time(second).

-spec seconds_since_epoch(seconds_since_epoch() | not_set) -> seconds_since_epoch().

seconds_since_epoch(not_set) ->
    os:system_time(second);
seconds_since_epoch(Seconds) ->
    Seconds.
