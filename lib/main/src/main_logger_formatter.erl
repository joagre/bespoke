% -*- fill-column: 100; -*-

-module(main_logger_formatter).
-behaviour(logger_formatter).
-export([check_config/1, format/2]).

-define(EMERGENCY, "\e[1;37;41m"). % Bold White on Red background
-define(ALERT,     "\e[1;30;41m"). % Bold Black on Red background
-define(CRITICAL,  "\e[1;31m").    % Bold Red
-define(ERROR,     "\e[1;91m").    % Bold Light Red
-define(WARNING,   "\e[1;33m").    % Bold Yellow
-define(NOTICE,    "\e[1;36m").    % Bold Cyan
-define(INFO,      "\e[1;32m").    % Bold Green
-define(DEBUG,     "\e[1;34m").    % Bold Blue
-define(RESET,     "\e[0m").       % Reset

check_config(_Config) ->
    ok.

format(Event, Config) ->
    Level = maps:get(level, Event, info),
    UppercaseLevel = string:to_upper(atom_to_list(Level)),
    Color = color(Level),
    Template = ["==", Color, UppercaseLevel, reset(Color), "== ", time, " ==",
                {module, [" in ", module, ":", function, "/", arity, " on line ", line, "\n"],
                 [" on line ", line, "\n"]},
                msg,
                {module, ["\n\n"], ["\n"]}],
    logger_formatter:format(Event, Config#{single_line => false, template => Template}).

color(emergency) -> ?EMERGENCY;
color(alert) -> ?ALERT;
color(critical) -> ?CRITICAL;
color(error) -> ?ERROR;
color(warning) -> ?WARNING;
color(notice) -> ?NOTICE;
color(info) -> ?INFO;
color(debug) -> ?DEBUG;
color(_) -> "".

reset("") -> "";
reset(_) -> ?RESET.
