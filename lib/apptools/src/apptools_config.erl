-module(apptools_config).
-export([lookup/4, insert/4]).
-export_type([key/0, value/0, error_reason/0]).

-include("../include/shorthand.hrl").

-type key() :: string().
-type value() :: string() | integer().
-type error_reason() :: {bad_format, string()} | file:posix() | badarg |
                        terminated | {no_translation, unicode, latin1}.

%%
%% Exported: lookup
%%

-spec lookup(string(), file:filename(), key(), value()) ->
          {ok, value()} | {error, error_reason()}.

lookup(KeyPrefix, ConfigFilename, Key, DefaultValue) ->
    {ok, File} = file:open(ConfigFilename, [read, raw, {read_ahead, 1024}]),
    Result = lookup_value(File, normalize_key(KeyPrefix ++ Key), DefaultValue),
    ok = file:close(File),
    Result.

normalize_key(Key) ->
    string:trim(Key).

lookup_value(File, Key, DefaultValue) ->
    case file:read_line(File) of
        {ok, Line} ->
            case parse_line(Line) of
                {ok, Key, Value} ->
                    {ok, Value};
                {ok, _, _} ->
                    lookup_value(File, Key, DefaultValue);
                ignored ->
                    lookup_value(File, Key, DefaultValue);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason};
        eof ->
            {ok, DefaultValue}
    end.

parse_line(Line) ->
    TrimmedLine = string:trim(Line),
    case TrimmedLine of
        [] ->
            ignored;
        [C|_] when C =:= $# ->
            ignored;
        _ ->
            case string:tokens(TrimmedLine, "=") of
                [Key|Rest] when Rest =/= [] ->
                    ValueStr = string:join(Rest, "="),
                    NormalizedKey = normalize_key(Key),
                    TrimmedValue = string:trim(ValueStr),
                    case parse_value(TrimmedValue) of
                        {ok, Value} ->
                            {ok, NormalizedKey, Value};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {error, {bad_format, Line}}
            end
    end.

parse_value([$"|_] = Value) ->
    IsEndQuote = lists:last(Value) =:= $",
    if
        length(Value) >= 2 andalso IsEndQuote ->
            Inner = lists:sublist(Value, 2, length(Value) - 2),
            {ok, unescape(Inner)};
        true ->
            {error, {bad_format, Value}}
    end;
parse_value(Value) ->
    case string:list_to_integer(Value) of
        {Int, []} ->
            {ok, Int};
        _ ->
            {error, {bad_format, Value}}
    end.

unescape([]) ->
    [];
unescape([$\\, Next|Rest]) ->
    case Next of
        $"  -> [$"| unescape(Rest)];
        $\\ -> [$\\| unescape(Rest)];
        $$  -> [$$ | unescape(Rest)];
        $`  -> [$` | unescape(Rest)];
        $\n -> unescape(Rest);
        _   -> [$\\, Next | unescape(Rest)]
    end;
unescape([C|Rest]) ->
    [C|unescape(Rest)].

%%
%% Exported: insert
%%

-spec insert(string(), file:filename(), key(), value()) ->
          ok | {error, error_reason()}.

insert(KeyPrefix, ConfigFilename, Key, Value) ->
    {ok, File} = file:open(ConfigFilename, [read, {read_ahead, 1024}]),
    {ok, TempFilename} = make_temp_filename(ConfigFilename),
    {ok, TempFile} = file:open(TempFilename, [write]),
    case insert_value(File, TempFile, normalize_key(KeyPrefix ++ Key), Value) of
        ok ->
            ok = file:close(File),
            ok = file:close(TempFile),
            file:rename(TempFilename, ConfigFilename);
        {error, Reason} ->
            ok = file:close(File),
            ok = file:close(TempFile),
            ok = file:delete(TempFilename),
            {error, Reason}
    end.

make_temp_filename(Filename) ->
    Rand = ?i2l(erlang:system_time(nanosecond) rem 1000000),
    {ok, filename:join(["tmp", Filename ++ "." ++ Rand ++ ".tmp"])}.

insert_value(File, TempFile, Key, NewValue) ->
    case file:read_line(File) of
        {ok, Line} ->
            case parse_line(Line) of
                {ok, Key, _OldValue} ->
                    NewLine = format_assignment(Key, NewValue),
                    ok = file:write(TempFile, NewLine ++ "\n"),
                    add_remaining_lines(File, TempFile);
                {ok, AnotherKey, AnotherValue} ->
                    NewLine = format_assignment(AnotherKey, AnotherValue),
                    ok = file:write(TempFile, NewLine ++ "\n"),
                    insert_value(File, TempFile, Key, NewValue);
                ignored ->
                    ok = file:write(TempFile, Line),
                    insert_value(File, TempFile, Key, NewValue);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason};
        eof ->
            NewLine = format_assignment(Key, NewValue),
            file:write(TempFile, NewLine ++ "\n")
    end.

format_assignment(Key, Value) when is_integer(Value) ->
    Key ++ "=" ++ ?i2l(Value);
format_assignment(Key, Value) when is_list(Value) ->
    Key ++ "=" ++ "\"" ++ escape_string(Value) ++ "\"".

escape_string(String) ->
    lists:flatten([escape_char(C) || C <- String]).

escape_char($")  -> [$\\, $" ];
escape_char($\\) -> [$\\, $\\];
escape_char($$)  -> [$\\, $$];
escape_char($`)  -> [$\\, $`];
escape_char(C)   -> [C].

add_remaining_lines(File, TempFile) ->
    case file:read_line(File) of
        {ok, Line} ->
            ok = file:write(TempFile, Line),
            add_remaining_lines(File, TempFile);
        {error, Reason} ->
            {error, Reason};
        eof ->
            ok
    end.
