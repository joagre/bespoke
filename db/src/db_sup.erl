-module(db_sup).
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
    DbServSpec = #{id => db_serv, start => {db_serv, start_link, []}},
    DbRestSpec = #{id => db_rest, start => {db_rest, start_link, []}},
    {ok, {#{strategy => one_for_all}, [DbServSpec, DbRestSpec]}}.
