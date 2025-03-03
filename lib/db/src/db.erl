% -*- fill-column: 100; -*-

-module(db).
-export([open_disk/3, lookup_disk/2, foldl_disk/3, insert_disk/2, delete_disk/2, dump_disk/1,
         sync_disk/1, close_disk/1]).
-export([open_disk_index/2, lookup_disk_index/2, insert_disk_index/3, delete_disk_index/2,
         delete_disk_index/3, dump_disk_index/1, sync_disk_index/1, close_disk_index/1]).
-export([open_ram/2, lookup_ram/2, foldl_ram/3, insert_ram/2, delete_ram/2, dump_ram/1,
         close_ram_db/1, seconds_since_epoch/0, seconds_since_epoch/1]).
-export_type([seconds_since_epoch/0]).

-type seconds_since_epoch() :: integer().

%%
%% Exported: open_disk_db
%%

-spec open_disk(dets:tab_name(), file:name(), integer()) ->
          {ok, dets:tab_name()} | {error, term()}.

open_disk(Name, Filename, KeyPos) ->
    dets:open_file(Name, [{file, Filename}, {keypos, KeyPos}]).

%%
%% Exported: lookup_disk
%%

-spec lookup_disk(dets:tab_name(), term()) -> [term()] | {error, term()}.

lookup_disk(Name, Key) ->
    dets:lookup(Name, Key).

%%
%% Exported: foldl_disk
%%

-spec foldl_disk(fun((term(), term()) -> term()), term(), dets:tab_name()) -> term().

foldl_disk(Fun, Acc, Name) ->
    dets:foldl(Fun, Acc, Name).

%%
%% Exported: insert_disk
%%

-spec insert_disk(dets:tab_name(), term()) -> ok | {error, term()}.

insert_disk(Name, Entry) ->
    dets:insert(Name, Entry).

%%
%% Exported: delete_disk
%%

-spec delete_disk(dets:tab_name(), term()) -> ok | {error, term()}.

delete_disk(Name, Key) ->
    dets:delete(Name, Key).

%%
%% Exported: dump_disk
%%

-spec dump_disk(dets:tab_name()) -> [term()].

dump_disk(Name) ->
    dets:foldl(fun(Entry, Acc) -> [Entry|Acc] end, [], Name).

%%
%% Exported: sync_disk
%%

-spec sync_disk(dets:tab_name()) -> ok | {error, term()}.

sync_disk(Name) ->
    dets:sync(Name).

%%
%% Exported: close_disk
%%

-spec close_disk(dets:tab_name()) -> ok | {error, term()}.

close_disk(Name) ->
    dets:close(Name).

%%
%% Exported: open_disk_index
%%

-spec open_disk_index(apptools_persistent_index:index_name(), file:name()) ->
          {ok, apptools_persistent_index:index_name()} | {error, term()}.

open_disk_index(Name, Filename) ->
    apptools_persistent_index:open(Name, Filename).

%%
%% Exported: looup_disk_index
%%

-spec lookup_disk_index(apptools_persistent_index:index_name(),
                        apptools_persistent_index:primary()) ->
          [apptools_persistent_index:secondary()] | {error, term()}.

lookup_disk_index(Name, Primary) ->
    apptools_persistent_index:lookup(Name, Primary).

%%
%% Exported: insert_disk_index
%%

-spec insert_disk_index(apptools_persistent_index:index_name(),
                        apptools_persistent_index:primary(),
                        apptools_persistent_index:secondary()) ->
          ok | {error, term()}.

insert_disk_index(Name, Primary, Secondary) ->
    apptools_persistent_index:insert(Name, Primary, Secondary).

%%
%% Exported: delete_disk_index
%%

-spec delete_disk_index(apptools_persistent_index:index_name(),
                        apptools_persistent_index:primary()) ->
          ok | {error, term()}.

delete_disk_index(Name, Primary) ->
    apptools_persistent_index:delete(Name, Primary).

-spec delete_disk_index(apptools_persistent_index:index_name(),
                        apptools_persistent_index:primary(),
                        apptools_persistent_index:secondary()) ->
          ok | {error, term()}.

delete_disk_index(Name, Primary, Secondary) ->
    apptools_persistent_index:delete(Name, Primary, Secondary).

%%
%% Exported: dump_disk_index
%%

-spec dump_disk_index(apptools_persistent_index:index_name()) -> [term()].

dump_disk_index(Name) ->
    apptools_persistent_index:dump(Name).

%%
%% Exported: sync_disk_index
%%

-spec sync_disk_index(apptools_persistent_index:index_name()) -> ok | {error, term()}.

sync_disk_index(Name) ->
    apptools_persistent_index:sync(Name).

%%
%% Exported: close_disk_index
%%

-spec close_disk_index(apptools_persistent_index:index_name()) -> ok | {error, term()}.

close_disk_index(Name) ->
    apptools_persistent_index:close(Name).

%%
%% Exported: open_ram
%%

-spec open_ram(atom(), integer()) -> ok.

open_ram(Name, KeyPos) ->
    Name = ets:new(Name, [{keypos, KeyPos}, named_table, public]),
    ok.

%%
%% Exported: lookup_ram
%%

-spec lookup_ram(atom(), term()) -> [term()].

lookup_ram(Name, Key) ->
    ets:lookup(Name, Key).

%%
%% Exported: foldl_ram
%%

-spec foldl_ram(fun((term(), term()) -> term()), term(), atom()) -> term().

foldl_ram(Fun, Acc, Name) ->
    ets:foldl(Fun, Acc, Name).

%%
%% Exported: insert_ram
%%

-spec insert_ram(atom(), term()) -> ok.

insert_ram(Name, Entry) ->
    true = ets:insert(Name, Entry),
    ok.

%%
%% Exported: delete_ram
%%

-spec delete_ram(atom(), term()) -> ok.

delete_ram(Name, Key) ->
    true = ets:delete(Name, Key),
    ok.

%%
%% Exported: dump_ram
%%

-spec dump_ram(atom()) -> [term()].

dump_ram(Name) ->
    ets:tab2list(Name).

%%
%% Exported: close_ram_db
%%

-spec close_ram_db(atom()) -> ok.

close_ram_db(Name) ->
    true = ets:delete(Name),
    ok.

%%
%% Exported: seconds_since_epoch
%%

-spec seconds_since_epoch() -> seconds_since_epoch().

seconds_since_epoch() ->
    os:system_time(second).

seconds_since_epoch(not_set) ->
    os:system_time(second);
seconds_since_epoch(Seconds) ->
    Seconds.
