% -*- fill-column: 100; -*-

-module(db_app).
-behaviour(application).
-export([start/2, stop/1]).

%%
%% Exported: start
%%

start(_Type, _StartArgs) ->
    case db_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%
%% Exported: stop
%%

stop(_State) ->
    ok.
