-module(elevator_control_server).
%% gen_server_mini_template
-behaviour(gen_server).
-export([start_link/0, register_elevator/1,user_call/2, stopped_at_floor/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

-record(state, {
	elevators = [],
	pending_up = ordsets:new(),
	pending_down = ordsets:new()
	}).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) -> {ok, #state{}}.


%%% External Calls
register_elevator(Pid) ->
	gen_server:call(?SERVER, {register_elevator, Pid}).

user_call(Floor, Direction) ->
	gen_server:call(?SERVER, {user_call, {Floor, Direction}}).

stopped_at_floor(Floor, Direction) ->
	gen_server:cast(?SERVER, {stopped_at_floor, {Floor, Direction}}).


%%% Gen_server Callbacks

handle_call({register_elevator, Pid}, _From, State) ->
	io:format("Registering elevator with PID ~p~n", [Pid]),
	{reply, ok, State#state{elevators = [Pid|State#state.elevators]} };
handle_call({user_call, {Floor, Direction}}, _From, State) ->
	io:format("User call from floor ~p Direction ~p~n", [Floor, Direction]),
	if 
		Direction == up ->
			NewState = State#state{pending_up=ordsets:add_element(Floor, State#state.pending_up)};
		true ->
			NewState = State#state{pending_down=ordsets:add_element(Floor, State#state.pending_down)}
	end,
	broadcast_call(NewState, Floor, Direction),
	{reply, ok, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({stopped_at_floor, {Floor, Direction}}, State) ->
	if 
		Direction == up ->
			NewSet = ordsets:add_element(Floor, State#state.pending_up),
			NewState = State#state{pending_up=NewSet};
		true -> % down
			NewSet = ordsets:add_element(Floor, State#state.pending_down),
			NewState = State#state{pending_down=NewSet}
	end,
	{noreply, NewState};
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% Internal Functions

broadcast_call(State, Floor, Direction) ->
	lists:foreach( fun(ElevatorPid) -> 
			ElevatorPid ! {destination, global, Floor, Direction}
		end, 
		State#state.elevators).