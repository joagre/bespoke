% -*- fill-column: 100; -*-

-module(db_meta_db).
-export([open/0, sync/0, close/0,
         read_host/0, read_next_user_id/0, read_next_message_id/0,
         read_next_message_attachment_id/0, read_next_post_id/0, read_next_file_id/0]).

-include("../include/db.hrl").
-include("db_meta_db.hrl").

%%
%% Exported: open
%%

-spec open() -> ok | {error, term()}.

open() ->
    {ok, _} = db:open_disk(?META_DB, ?META_FILE_PATH, #meta.type),
    case dets:lookup(?META_DB, basic) of
        [] ->
            dets:insert(?META_DB, #meta{});
        [_] ->
            ok
    end.

%%
%% Exported: sync
%%

-spec sync() -> ok | {error, term()}.

sync() ->
    db:sync_disk_index(?META_DB).

%%
%% Exported: close
%%

-spec close() -> ok | {error, term()}.

close() ->
    db:close_disk_index(?META_DB).

%%
%% Exported: read_host
%%

-spec read_host() -> db_serv:host() | not_set.

read_host() ->
    [#meta{host = Host}] = dets:lookup(?META_DB, basic),
    Host.

%%
%% Exported: read_next_user_id
%%

-spec read_next_user_id() -> db_serv:user_id().

read_next_user_id() ->
    step_meta_id(#meta.next_user_id).

%%
%% Exported: read_next_message_id
%%

-spec read_next_message_id() -> db_serv:message_id().

read_next_message_id() ->
    step_meta_id(#meta.next_message_id).

%%
%% Exported: read_next_message_attachment_id
%%

-spec read_next_message_attachment_id() -> db_serv:message_attachment_id().

read_next_message_attachment_id() ->
    step_meta_id(#meta.next_message_attachment_id).

%%
%% Exported: read_next_post_id
%%

-spec read_next_post_id() -> integer().

read_next_post_id() ->
    step_meta_id(#meta.next_post_id).

%%
%% Exported: read_next_file_id
%%

-spec read_next_file_id() -> db_serv:file_id().

read_next_file_id() ->
    step_meta_id(#meta.next_file_id).

%%
%% Utilities
%%

step_meta_id(N) ->
    [Meta] = dets:lookup(?META_DB, basic),
    NextId = element(N, Meta),
    UpdatedMeta = setelement(N, Meta, NextId + 1),
    ok = dets:insert(?META_DB, UpdatedMeta),
    NextId.
