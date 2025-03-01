% -*- fill-column: 100; -*-

-module(main).
-export([start/0, stop/0, lookup_config/2, insert_config/2]).
-export_type([filename/0, file_path/0]).

-include_lib("apptools/include/shorthand.hrl").
-include_lib("db/include/db.hrl").

-define(CONFIG_FILE, filename:join(?BESPOKE_RUNTIME_DIR, "bespoke.conf")).
-define(LOG_FILE, filename:join(?BESPOKE_RUNTIME_DIR, "log/bespoke.log")).

-type filename() :: binary().
-type file_path() :: binary().

%%
%% Exported: start
%%

-spec start() -> ok.

start() ->
    {ok, _} = application:ensure_all_started(sasl),
    ok = configure_logger(),
    {ok, _} = application:ensure_all_started(ssl),
    ok = application:start(apptools),
    ok = apptools_mime:start(),
    ok = application:start(rester),
    ok = application:start(db),
    ok = set_ssid(),
    ok = application:start(webapp).

configure_logger() ->
    ok = logger:remove_handler(default),
    ok = add_bespoke_logger(io_logger, #{type => standard_io}),
    add_bespoke_logger(file_logger, #{file => ?LOG_FILE}).

add_bespoke_logger(Name, Config) ->
    logger:add_handler(
      Name, logger_std_h,
      #{config => Config,
        formatter =>
            {logger_formatter,
             #{single_line => false,
               template =>
                   ["=", level, "==== ", time, " ===",
                    {module,
                     [" in ", module,":", function, "/", arity, " on line ", line, "\n"],
                     [" on line ", line, "\n"]},
                    msg, {module, ["\n\n"], ["\n"]}]}}}).

set_ssid() ->
    {ok, SSID} = lookup_config("SSID", "BespokeBBS"),
    %% In case the SSID is not set in the config file, we set it to the default
    ok = insert_config("SSID", SSID),
    _ = webapp_rest:change_ssid(?l2b(SSID)),
    ok.

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    init:stop().

%%
%% Exported: lookup_config
%%

-spec lookup_config(apptools_config:key(), apptools_config:value()) ->
          {ok, apptools_config:value()} |
          {error, apptools_config:error_reason()}.

lookup_config(Key, DefaultValue) ->
    apptools_config:lookup("Bespoke", ?CONFIG_FILE, Key, DefaultValue).

%%
%% Exported: insert_config
%%

-spec insert_config(apptools_config:key(), apptools_config:value()) ->
          ok | {error, apptools_config:error_reason()}.

insert_config(Key, Value) ->
    apptools_config:insert("Bespoke", ?CONFIG_FILE, Key, Value).
