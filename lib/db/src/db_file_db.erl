% -*- fill-column: 100; -*-

-module(db_file_db).
-export([open/0, dump/0, sync/0, close/0, create_file/1, read_files/0, read_files/1, delete_file/1,
         file_is_uploaded/1]).

-include_lib("kernel/include/file.hrl").
-include_lib("apptools/include/log.hrl").
-include("../include/db.hrl").

%% File DB
-define(FILE_FILE_PATH, filename:join(?BESPOKE_DB_DIR, "file.db")).
-define(FILE_DB, file_db).

%%
%% Exported: open
%%

-spec open() -> ok.

open() ->
    {ok, _} = dets:open_file(?FILE_DB, [{file, ?FILE_FILE_PATH}, {keypos, #file.id}]),
    ok.

%%
%% Exported: dump
%%

-spec dump() -> [{dets:tab_name(), [#file{}]}].

dump() ->
    [{?FILE_DB, db:dets_dump(?FILE_DB)}].

%%
%% Exported: sync
%%

-spec sync() -> ok | {error, term()}.

sync() ->
    dets:sync(?FILE_DB).

%%
%% Exported: close
%%

-spec close() -> ok | {error, term()}.

close() ->
    dets:close(?FILE_DB).

%%
%% Exported: create_file
%%

-spec create_file(#file{}) -> {ok, #file{}} | {error, term()}.

create_file(File) ->
    FileId = db_meta_db:read_next_file_id(),
    UpdatedFile = File#file{id = FileId, created = db:seconds_since_epoch()},
    case dets:insert(?FILE_DB, UpdatedFile) of
        ok ->
            {ok, UpdatedFile};
        {error, Reason} ->
            {error, Reason}
    end.

%%
%% Exported: read_files
%%

-spec read_files() -> [{#file{}}].

read_files() ->
    Files = dets:foldl(fun(#file{is_uploading = true} = File, Acc) ->
                               [update_uploaded_size(File)|Acc];
                          (File, Acc) ->
                               [File|Acc]
                       end, [], ?FILE_DB),
    sort_files(Files).

update_uploaded_size(#file{id = FileId, is_uploading = true} = File) ->
    TmpPath = ?BESPOKE_TMP_PATH,
    TmpFilePathBeginning = filename:join([TmpPath, io_lib:format("file-~w-", [FileId])]),
    case filelib:wildcard(TmpFilePathBeginning ++ "*") of
        [TmpFilePath|_] ->
            case file:read_file_info(TmpFilePath) of
                {ok, FileInfo} ->
                    File#file{uploaded_size = FileInfo#file_info.size};
                {error, _} ->
                    File
            end;
        [] ->
            File
    end;
update_uploaded_size(File) ->
    File.

%%
%% Exported: read_files
%%

-spec read_files([db_serv:file_id()]) -> [#file{}].

read_files(FileIds) ->
    Files = lists:foldr(
              fun(FileId, Acc) ->
                      [File] = dets:lookup(?FILE_DB, FileId),
                      [update_uploaded_size(File)|Acc]
              end, [], FileIds),
    sort_files(Files).

%%
%% Exported: delete_file
%%

-spec delete_file(db_serv:file_id()) -> ok | {error, not_found}.

delete_file(FileId) ->
    case dets:lookup(?FILE_DB, FileId) of
        [File] ->
            ok = dets:delete(?FILE_DB, FileId),
            ok = delete_file_on_disk(File);
        [] ->
            {error, not_found}
    end.

delete_file_on_disk(#file{id = FileId, filename = Filename}) ->
    TmpPath = ?BESPOKE_TMP_PATH,
    TmpFilePathBeginning = filename:join([TmpPath, io_lib:format("file-~w-", [FileId])]),
    case filelib:wildcard(TmpFilePathBeginning ++ "*") of
        [TmpFilePath] ->
            ?log_info("Deleting ~s (if it exists)...", [TmpFilePath]),
            _ = file:delete(TmpFilePath),
            UploadedFilePath =
                filename:join([?BESPOKE_FILE_PATH, io_lib:format("~w-~s", [FileId, Filename])]),
            ?log_info("Deleting ~s (if it exists)...", [UploadedFilePath]),
            _ = file:delete(UploadedFilePath),
            ok;
        [] ->
            ok
    end.

%%
%% Exported: file_is_uploaded
%%

-spec file_is_uploaded(db_serv:file_id()) -> ok | {error, not_found}.

file_is_uploaded(FileId) ->
    case dets:lookup(?FILE_DB, FileId) of
        [File] ->
            UpdatedFile = File#file{is_uploading = false},
            ok = dets:insert(?FILE_DB, UpdatedFile),
            ok = move_file_on_disk(UpdatedFile);
        [] ->
            {error, not_found}
    end.

move_file_on_disk(#file{id = FileId, filename = Filename}) ->
    TmpPath = ?BESPOKE_TMP_PATH,
    TmpFilePathBeginning = filename:join([TmpPath, io_lib:format("file-~w-", [FileId])]),
    case filelib:wildcard(TmpFilePathBeginning ++ "*") of
        [TmpFilePath] ->
            UploadedFilePath =
                filename:join([?BESPOKE_FILE_PATH, io_lib:format("~w-~s", [FileId, Filename])]),
            ?log_info("Moving ~s to ~s", [TmpFilePath, UploadedFilePath]),
            ok = file:rename(TmpFilePath, UploadedFilePath);
        [] ->
            ok
    end.

%%
%% Utilities
%%

sort_files(Files) ->
    lists:sort(fun(FileA, FileB) ->
                       FileA#file.created > FileB#file.created
               end, Files).
