%%%-------------------------------------------------------------------
%%% @author some name  <me@hostname.local>
%%% @copyright (C) 2013, some name
%%% @doc
%%%
%%% @end
%%% Created : 26 May 2013 by some name <me@hostname.local>
%%%-------------------------------------------------------------------
-module(ecs_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    ECS = {'Elevator Control Server', {elevator_control_server, start_link, []},
        Restart, Shutdown, worker, [elevator_control_server]},
    ElevatorSupervisor = {'Elevator Supervisor', {elevator_supervisor, start_link, []},
        Restart, Shutdown, supervisor, [elevator_supervisor]},
    TimeServer = {'Time Server', {timing_server, start_link, []},
        Restart, Shutdown, worker, [timing_server]},
    io:format("Starting Elevators Parent Supervisor~n", []),

    {ok, {SupFlags, [ECS, ElevatorSupervisor, TimeServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================