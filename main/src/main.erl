-module(main).
-export([start/0, stop/0]).

%%
%% Exported: start
%%

-spec start() -> ok.

start() ->
    ok = application:start(sasl),
    {ok, _} = application:ensure_all_started(ssl),
    ok = application:start(apptools),
    ok = application:start(rester),
    ok = application:start(db).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    init:stop().
