-module(main).
-export([start/0, stop/0]).

%%
%% Exported: start
%%

-spec start() -> ok.

start() ->
    {ok, _} = application:ensure_all_started(sasl),
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
