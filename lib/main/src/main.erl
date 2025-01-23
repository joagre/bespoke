-module(main).
-export([start/0, stop/0]).

%%
%% Exported: start
%%

-spec start() -> ok.

start() ->
    {ok, _} = application:ensure_all_started(sasl),
    %%ok = logger:remove_handler(default),
    ok = logger:add_handler(
           file_logger, logger_std_h,
           #{config => #{file => "/var/tmp/bespoke/log/bespoke.log"}}),
    {ok, _} = application:ensure_all_started(ssl),
    ok = application:start(apptools),
    ok = application:start(rester),
    ok = application:start(db),
    ok = application:start(webapp).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    init:stop().
