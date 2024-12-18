-module(serv_manager).
-export([start_link/0, stop/0, add_process/1, reload_processes/1]).
-export([message_handler/1]).

-include_lib("apptools/include/log.hrl").
-include_lib("apptools/include/serv.hrl").

%%
%% Exported: start_link
%%

-spec start_link() ->
          serv:spawn_server_result().

start_link() ->
    ?spawn_server(
      fun init/1,
      fun message_handler/1,
      #serv_options{name = ?MODULE}).

%%
%% Exported: stop
%%

-spec stop() -> ok.

stop() ->
    serv:call(?MODULE, stop).

%%
%% Exported: subscribe
%%

-spec add_process(atom()) -> ok | {error, manager_not_started}.

add_process(Pid) ->
    case whereis(?MODULE) of
        ServPid when is_pid(ServPid) ->
            serv:cast(ServPid, {add_process, Pid});
        undefined ->
            {error, manager_not_started}
    end.

%%
%% Exported: reload_processess
%%

-spec reload_processes(atom()) -> ok | manager_not_started.

reload_processes(ModuleName) ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            serv:cast(Pid, {reload_processes, ModuleName});
        undefined ->
            {error, manager_not_started}
    end.

%%
%% Server
%%

init(Parent) ->
    ?log_info("Serv Manager has been started"),
    {ok, #{parent => Parent, processes => #{}}}.

message_handler(#{parent := Parent, processes := Processes} = State) ->
    receive
        {call, From, stop = Call} ->
            ?log_debug("Call: ~p", [Call]),
            {stop, From, ok};
        {cast, {add_process, Pid} = Cast} ->
            ?log_debug("Cast: ~p", [Cast]),
            MonitorRef = monitor(process, Pid),
            {noreply, State#{processes =>
                                 maps:put(Pid, MonitorRef, Processes)}};
        {cast, {reload_processes, _ModuleName} = Cast} ->
            ?log_debug("Cast: ~p", [Cast]),
            ok = calm_down(),
            ok = maps:fold(
                   fun(Pid, _MonitorRef, ok) ->
                           ?log_debug("Reload: ~w", [Pid]),
                           catch begin
                                     Pid ! {system, undefined, code_switch}
                                 end,
                           ok
                   end, ok, Processes),
            noreply;
        {'DOWN', _Ref, process, Pid, Info} ->
            ?log_debug("DOWN: ~p", [Info]),
            {noreply, State#{processes => maps:remove(Pid, Processes)}};
        {system, From, Request} ->
            ?log_debug("System: ~p", [Request]),
            {system, From, Request};
        {'EXIT', Parent, Reason} ->
            exit(Reason);
        UnknownMessage ->
            ?log_error("Unknown message: ~p", [UnknownMessage]),
            noreply
    end.

calm_down() ->
    receive
        {cast, {reload_processes, _ModuleName}} ->
            calm_down()
    after 200 ->
            ok
    end.
