-module(apptools_persistent_index).
-export([open/1, open/2, insert/3, lookup/2, delete/2, close/1]).
-export_type([index_name/0, index_tid/0, index/0, primary/0, secondary/0]).

-type index_name() :: dets:tab_name().
-type index_tid() :: reference().
-type index() :: index_name() | index_tid().
-type primary() :: any().
-type secondary() :: any().

%%
%% Exported: open
%%

-spec open(file:name()) -> {ok, index_tid()} | {error, term()}.

open(Filename) ->
    dets:open_file(Filename, [{type, bag}]).

-spec open(index_name(), file:name()) -> {ok, index_name()} | {error, term()}.

open(Name, Filename) ->
    dets:open_file(Name, [{type, bag}, {file, Filename}]).

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

%%
%% Exported: close
%%

-spec close(index()) -> ok | {error, term()}.

close(Index) ->
    dets:close(Index).
