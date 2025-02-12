-module(apptools_persistent_key_index).
-export([open/1, open/2, insert/3, lookup/2, delete/2, close/1]).
-export_type([index_name/0, index_tid/0, index/0, key/0, id/0]).

-include("../include/persistent_key_index.hrl").

-type index_name() :: dets:tab_name().
-type index_tid() :: reference().
-type index() :: index_name() | index_tid().
-type key() :: any().
-type id() :: any().

%%
%% Exported: open
%%

-spec open(file:name()) -> {ok, index_tid()} | {error, term()}.

open(IndexPath) ->
    dets:open_file(IndexPath).

-spec open(index_name(), file:name()) -> {ok, index_name()} | {error, term()}.

open(IndexName, IndexPath) ->
    dets:open_file(IndexName, [{file, IndexPath}]).

%%
%% Exported: insert
%%

-spec insert(index(), key(), id()) -> ok | {error, term()}.

insert(Index, Key, Id) ->
    dets:insert(Index, #index{key = Key, id = Id}).

%%
%% Exported: lookup
%%

-spec lookup(index(), key()) -> id() | {error, term()}.

lookup(Index, Key) ->
    case dets:lookup(Index, Key) of
        {error, Reason} ->
            {error, Reason};
        [#index{id = Id}] ->
            Id
    end.

%%
%% Exported: delete
%%

-spec delete(index(), key()) -> ok | {error, term()}.

delete(Index, Key) ->
    dets:delete(Index, Key).

%%
%% Exported: close
%%

-spec close(index()) -> ok | {error, term()}.

close(Index) ->
    dets:close(Index).
