% -*- fill-column: 100; -*-

-module(apptools_mime).
-export([start/0, stop/0, mime_type/1]).

-define(MIME_TYPES_FILE, <<"/etc/mime.types">>).
-define(MIME_TYPES_DB, mime_types_db).

%%
%% Exported: start
%%

-spec start() -> ok | {error, file:posix()}.

start() ->
    case ets:whereis(?MIME_TYPES_DB) of
        undefined ->
            case file:open(?MIME_TYPES_FILE, [read, binary, {read_ahead, 1024}]) of
                {ok, Fd} ->
                    ?MIME_TYPES_DB = ets:new(?MIME_TYPES_DB, [named_table, public]),
                    start(Fd);
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            ok
    end.

start(Fd) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            case parse_line(string:trim(Line)) of
                {ok, MimeType, Extensions} ->
                    lists:foreach(fun(Extension) ->
                                          true = ets:insert(?MIME_TYPES_DB, {Extension, MimeType})
                                  end, Extensions),
                    start(Fd);
                ignored ->
                    start(Fd)
            end;
        {error, Reason} ->
            ok = file:close(Fd),
            {error, Reason};
        eof ->
            file:close(Fd)
    end.

parse_line(<<"#", _/binary>>) ->
    ignored;
parse_line(Line) ->
    case string:lexemes(Line, "\t ") of
        [MimeType|Extensions] when length(Extensions) > 0 ->
            {ok, MimeType, Extensions};
        _ ->
            ignored
    end.

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    case ets:whereis(?MIME_TYPES_DB) of
        undefined ->
            ok;
        _ ->
            true = ets:delete(?MIME_TYPES_DB),
            ok
    end.

%%
%% Exported: mime_type
%%

-spec mime_type(binary()) -> {ok, binary()} | {error, not_found}.

mime_type(Extension) ->
    case ets:lookup(?MIME_TYPES_DB, Extension) of
        [] ->
            {error, not_found};
        [{_, MimeType}] ->
            {ok, MimeType}
    end.
