% -*- fill-column: 100; -*-

-module(main_logger_formatter).
-behaviour(logger_formatter).
-export([check_config/1, format/2]).

-define(RED, "\e[31m").
-define(YELLOW, "\e[33m").
-define(GREEN, "\e[32m").
-define(RESET, "\e[0m").

check_config(_Config) ->
    ok.

format(Event, Config) ->
    Level = maps:get(level, Event, info),
    Color = case Level of
                error -> ?RED;
                info -> ?YELLOW;
                debug -> ?GREEN;
                _ -> ""
            end,
    Reset = if Color =:= "" -> "";
               true -> ?RESET
            end,
    Template = ["==", Color, level, Reset, "== ", time, " ==",
                {module, [" in ", module, ":", function, "/", arity, " on line ", line, "\n"],
                 [" on line ", line, "\n"]},
                msg,
                {module, ["\n\n"], ["\n"]}],
    logger_formatter:format(Event, Config#{single_line => false, template => Template}).
