% -*- fill-column: 100; -*-

-module(webapp_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]). %% Used by supervisor:start_link/2

%%
%% Exported: start_link
%%

-spec start_link() -> any().

start_link() ->
    case supervisor:start_link(?MODULE, []) of
        {ok, SupervisorPid} ->
            {ok, SupervisorPid};
        Error ->
            Error
    end.

%%
%% Exported: init
%%

init([]) ->
    WebappRestSpec = #{id => webapp_rest, start => {webapp_rest, start_link, []}},
    WebappSessionServSpec = #{id => webapp_session_serv,
                              start => {webapp_session_serv, start_link, []}},
    {ok, {#{strategy => one_for_all}, [WebappRestSpec, WebappSessionServSpec]}}.
