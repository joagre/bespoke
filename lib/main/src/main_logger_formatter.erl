% -*- fill-column: 100; -*-

-module(main_logger_formatter).
-behaviour(logger_formatter).
-export([check_config/1, format/2]).

-define(ERROR, "\e[1;31m"). % Red
-define(INFO, "\e[1;32m"). % Green
-define(RESET, "\e[0m").

check_config(_Config) ->
    ok.

format(Event, Config) ->
    Level = maps:get(level, Event, info),
    UppercaseLevel = string:to_upper(atom_to_list(Level)),
    Color = case Level of
                error -> ?ERROR;
                info -> ?INFO;
                _ -> ""
            end,
    Reset = if
                Color =:= "" -> "";
                true -> ?RESET
            end,
    Template = ["==", Color, UppercaseLevel, Reset, "== ", time, " ==",
                {module, [" in ", module, ":", function, "/", arity, " on line ", line, "\n"],
                 [" on line ", line, "\n"]},
                msg,
                {module, ["\n\n"], ["\n"]}],
    logger_formatter:format(Event, Config#{single_line => false, template => Template}).
