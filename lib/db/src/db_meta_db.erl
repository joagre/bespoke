% -*- fill-column: 100; -*-

-module(db_meta_db).
-export([open/0, dump/0, sync/0, close/0,
         read_host/0, read_next_user_id/0, read_next_message_id/0, read_next_attachment_id/0,
         read_next_post_id/0, read_next_file_id/0]).

-include("../include/db.hrl").

-define(META_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "meta.db")).
-define(META_DB, meta_db).

%%
%% Exported: open
%%

-spec open() -> ok | {error, term()}.

open() ->
    {ok, _} = dets:open_file(?META_DB, [{file, ?META_FILE_PATH}, {keypos, #meta.type}]),
    case dets:lookup(?META_DB, basic) of
        [] ->
            dets:insert(?META_DB, #meta{});
        [_] ->
            ok
    end.

%%
%% Exported: dump
%%

-spec dump() -> [{dets:tab_name(), [#meta{}]}].

dump() ->
    [Meta] = dets:lookup(?META_DB, basic),
    [{?META_DB, [Meta]}].

%%
%% Exported: sync
%%

-spec sync() -> ok | {error, term()}.

sync() ->
    dets:sync(?META_DB).

%%
%% Exported: close
%%

-spec close() -> ok | {error, term()}.

close() ->
    dets:close(?META_DB).

%%
%% Exported: read_host
%%

-spec read_host() -> db:host() | not_set.

read_host() ->
    [#meta{host = Host}] = dets:lookup(?META_DB, basic),
    Host.

%%
%% Exported: read_next_user_id
%%

-spec read_next_user_id() -> db:user_id().

read_next_user_id() ->
    step_meta_id(#meta.next_user_id).

%%
%% Exported: read_next_message_id
%%

-spec read_next_message_id() -> db:message_id().

read_next_message_id() ->
    step_meta_id(#meta.next_message_id).

%%
%% Exported: read_next_attachment_id
%%

-spec read_next_attachment_id() -> db:attachment_id().

read_next_attachment_id() ->
    step_meta_id(#meta.next_attachment_id).

%%
%% Exported: read_next_post_id
%%

-spec read_next_post_id() -> integer().

read_next_post_id() ->
    step_meta_id(#meta.next_post_id).

%%
%% Exported: read_next_file_id
%%

-spec read_next_file_id() -> db:file_id().

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
