% -*- fill-column: 100; -*-

-module(db).
-export([dets_dump/1, seconds_since_epoch/0, seconds_since_epoch/1]).
-export_type([host/0, user_id/0, message_id/0, attachment_id/0, file_id/0, post_id/0, username/0,
              title/0, body/0, content_type/0, file_size/0, seconds_since_epoch/0]).

-type host() :: binary().
-type user_id() :: integer().
-type message_id() :: integer().
-type attachment_id() :: integer().
-type file_id() :: integer().
-type post_id() :: binary().
-type username() :: binary().
-type title() :: binary().
-type body() :: binary().
-type content_type() :: binary().
-type file_size() :: integer().
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
