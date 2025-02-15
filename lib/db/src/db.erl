-module(db).
-export([open_disk_db/3, sync_disk_db/1, close_disk_db/1,
         open_disk_index_db/2, sync_disk_index_db/1, close_disk_index_db/1,
         open_ram_db/2]).

%%
%% Exported: open_disk_db
%%

-spec open_disk_db(dets:tab_name(), file:name(), integer()) ->
          {ok, dets:tab_name()} | {error, term()}.

open_disk_db(Name, Filename, KeyPos) ->
    dets:open_file(Name, [{file, Filename}, {keypos, KeyPos}]).

%%
%% Exported: sync_disk_db
%%

-spec sync_disk_db(dets:tab_name()) -> ok | {error, term()}.

sync_disk_db(Name) ->
    dets:sync(Name).

%%
%% Exported: close_disk_db
%%

-spec close_disk_db(dets:tab_name()) -> ok | {error, term()}.

close_disk_db(Name) ->
    dets:close(Name).

%%
%% Exported: open_disk_index_db
%%

-spec open_disk_index_db(apptools_persistent_index:index_name(), file:name()) ->
          {ok, apptools_persistent_index:index_name()} | {error, term()}.

open_disk_index_db(Name, Filename) ->
    apptools_persistent_index:open(Name, Filename).

%%
%% Exported: sync_disk_index_db
%%

-spec sync_disk_index_db(apptools_persistent_index:index_name()) ->
          ok | {error, term()}.

sync_disk_index_db(Name) ->
    apptools_persistent_index:sync(Name).

%%
%% Exported: close_disk_index_db
%%

-spec close_disk_index_db(apptools_persistent_index:index_name()) ->
          ok | {error, term()}.

close_disk_index_db(Name) ->
    apptools_persistent_index:close(Name).

%%
%% Exported: open_ram_db
%%

-spec open_ram_db(atom(), integer()) -> ok.

open_ram_db(Name, KeyPos) ->
    Name = ets:new(Name, [{keypos, KeyPos}, named_table, public]),
    ok.
