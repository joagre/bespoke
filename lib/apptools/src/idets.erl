% -*- fill-column: 100; -*-

-module(idets).
-export([open_file/1, open_file/2, sync/1, insert/3, lookup/2, delete/2, delete/3, dump/1,
         close/1]).
-export_type([index_name/0, index_tid/0, index/0, primary/0, secondary/0]).

-type index_name() :: dets:tab_name().
-type index_tid() :: reference().
-type index() :: index_name() | index_tid().
-type primary() :: term().
-type secondary() :: term().

%%
%% Exported: open
%%

-spec open_file(file:name()) -> {ok, index_tid()} | {error, term()}.

open_file(Filename) ->
    dets:open_file(Filename, [{type, bag}]).

-spec open_file(index_name(), file:name()) -> {ok, index_name()} | {error, term()}.

open_file(Name, Filename) ->
    dets:open_file(Name, [{type, bag}, {file, Filename}]).

%%
%% Exported: sync
%%

-spec sync(index()) ->  ok | {error, term()}.

sync(Index) ->
    dets:sync(Index).

%%
%% Exported: insert
%%

-spec insert(index(), primary(), secondary()) -> ok | {error, term()}.

insert(Index, Primary, Secondary) ->
    dets:insert(Index, {Primary, Secondary}).

%%
%% Exported: lookup
%%

-spec lookup(index(), primary()) -> [secondary()] | {error, term()}.

lookup(Index, Primary) ->
    case dets:lookup(Index, Primary) of
        {error, Reason} ->
            {error, Reason};
        List ->
            lists:map(fun({_Primary, Secondary}) -> Secondary end, List)
    end.

%%
%% Exported: delete
%%

-spec delete(index(), primary()) -> ok | {error, term()}.

delete(Index, Primary) ->
    dets:delete(Index, Primary).

-spec delete(index(), primary(), secondary()) -> ok | {error, term()}.

delete(Index, Primary, Secondary) ->
    dets:delete_object(Index, {Primary, Secondary}).

%%
%% dump
%%

-spec dump(index()) -> [term()].

dump(Index) ->
    dets:foldl(fun(Entry, Acc) -> [Entry|Acc] end, [], Index).

%%
%% Exported: close
%%

-spec close(index()) -> ok | {error, term()}.

close(Index) ->
    dets:close(Index).
