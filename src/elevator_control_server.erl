-module(elevator_control_server).
%% gen_server_mini_template
-behaviour(gen_server).
-export([start_link/0, register_elevator/3,user_call/2,user_call_cancel/2,
	stopped_at_floor/1, elevator_local_destination/2,
elevator_local_destination_cancel/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

-include("states.hrl").

-record(state, {
	elevators = dict:new(),
	elevatorStates = dict:new(),
	pending_up = ordsets:new(),
	pending_down = ordsets:new()
	}).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) -> {ok, #state{}}.


%%% External Calls
register_elevator(Name, Pid, ElevatorState) ->
	gen_server:call(?SERVER, {register_elevator, {Name, Pid, ElevatorState}}).

user_call(Floor, Direction) ->
	gen_server:call(?SERVER, {user_call, {Floor, Direction}}).
user_call_cancel(Floor, Direction) ->
	gen_server:call(?SERVER, {user_call_cancel, {Floor, Direction}}).

stopped_at_floor(State) ->
	gen_server:call(?SERVER, {stopped_at_floor, {State}}).

elevator_local_destination(ElevatorName, Floor) ->
	gen_server:call(?SERVER, {local_destination, {ElevatorName, Floor}}). 
elevator_local_destination_cancel(ElevatorName, Floor) ->
	gen_server:call(?SERVER, {local_destination_cancel, {ElevatorName, Floor}}). 

%%% Gen_server Callbacks

handle_call({register_elevator, {Name, Pid, ElevatorState}}, _From, State) ->
	io:format("ECS: Registering elevator with PID ~p~n", [Pid]),
	NewElevators = dict:store(Name, Pid, State#state.elevators),
	NewElevatorStates = dict:store(Pid, ElevatorState, State#state.elevators),
	{reply, ok, State#state{elevators=NewElevators, elevatorStates=NewElevatorStates}};
handle_call({user_call, {Floor, Direction}}, _From, State) ->
	io:format("ECS: User call from floor ~p Direction ~p~n", [Floor, Direction]),
	if 
		Direction =:= up ->
			NewState = State#state{pending_up=ordsets:add_element(Floor, State#state.pending_up)};
		true ->
			NewState = State#state{pending_down=ordsets:add_element(Floor, State#state.pending_down)}
	end,
	broadcast_call(NewState, Floor, Direction),
	{reply, ok, NewState};
handle_call({user_call_cancel, {Floor, Direction}}, _From, State) ->
	io:format("ECS: User call cancel from floor ~p Direction ~p~n", [Floor, Direction]),
	if 
		Direction =:= up ->
			NewState = State#state{pending_up=ordsets:del_element(Floor, State#state.pending_up)};
		true ->
			NewState = State#state{pending_down=ordsets:del_element(Floor, State#state.pending_down)}
	end,
	{reply, ok, NewState};
handle_call({stopped_at_floor, {ElevatorState}}, {From, _}, OldState) -> 
	Floor = ElevatorState#elevatorState.floor,
	Direction = ElevatorState#elevatorState.direction,
	io:format("ECS: Elevator checking if pending at floor ~p Direction ~p; up:~p, down:~p~n", [Floor, Direction, OldState#state.pending_up, OldState#state.pending_down]),
	State = dict:store(From, ElevatorState, OldState),
	if 
		Direction =:= up ->
			Resp = ordsets:is_element(Floor, State#state.pending_up),
			if 
				Resp ->
					NewSet = ordsets:del_element(Floor, State#state.pending_up),
					interface_server:status_update({floor_consumed, Floor, Direction}),
					NewState = State#state{pending_up=NewSet};
				true ->
					NewState = State
			end;
		true ->
			Resp = ordsets:is_element(Floor, State#state.pending_down),
			if 
				Resp ->
					NewSet = ordsets:del_element(Floor, State#state.pending_down),
					interface_server:status_update({floor_consumed, Floor, Direction}),
					NewState = State#state{pending_down=NewSet};
				true ->
					NewState = State
			end
	end,
	io:format("ECS: Answer is ~p~n", [Resp]),
	{reply, Resp, NewState};
handle_call({local_destination, {ElevatorName, Floor}}, _From, State) ->
	io:format("Local destination called in elevator ~p for floor ~p~n", [ElevatorName, Floor]),
	Elevator = dict:fetch(ElevatorName, State#state.elevators),
	Elevator ! {destination, local, Floor, ok},
	{reply, ok, State};
handle_call({local_destination_cancel, {ElevatorName, Floor}}, _From, State) ->
	io:format("Local destination cancel called in elevator ~p for floor ~p~n", [ElevatorName, Floor]),
	Elevator = dict:fetch(ElevatorName, State#state.elevators),
	Elevator ! {destination_cxl, local, Floor, ok},
	{reply, ok, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% Internal Functions

broadcast_call(State, Floor, Direction) ->
	%Need to pick the best elevator., but if one isnt available jsut leave it
	% lists:foreach( fun({_Name, ElevatorPid}) -> 
	% 		ElevatorPid ! {destination, global, Floor, Direction}
	% 	end, 
	% 	dict:to_list(State#state.elevators) ),

	case 
		get_closest_valid(dict:to_list(State#state.elevatorStates), {none, -1}, {Floor, Direction})
		 	of
		 {ok, {ElevatorPid, _}} ->
		 	ElevatorPid ! {destination, global, Floor, Direction};
		 {not_found} ->
		 	ok
	end.

get_closest_valid([], {_ClosestElevator, -1}, _CallDetails) ->
	{not_found};
get_closest_valid([], {ClosestElevator, _}, _CallDetails) ->
	{ok, ClosestElevator};	
get_closest_valid([{Pid, ElevatorState}|L], {ClosestElevator, ClosestDistance}, {Floor, Direction}) ->
	%% IF direction of elevator matches button direction or is none:
	%	check the distance and replace if distance is shorter then loop
	Distance = get_distance(Floor, ElevatorState#elevatorState.floor),
	if ClosestDistance == -1 ->
		case lists:member(ElevatorState#elevatorState.direction, [Direction, none]) of
			true ->
				% is best
				get_closest_valid(L, {{Pid, ElevatorState}, Distance}, {Floor, Direction});
			false ->
				% move on
				get_closest_valid(L, {ClosestElevator, ClosestDistance}, {Floor, Direction})
		end;
	true ->
		case lists:member(ElevatorState#elevatorState.direction, [Direction, none]) of
			true when Distance < ClosestDistance ->
				% is best
				get_closest_valid(L, {{Pid, ElevatorState}, Distance}, {Floor, Direction});
			_ ->
				% move on
				get_closest_valid(L, {ClosestElevator, ClosestDistance}, {Floor, Direction})
		end
	end.
get_distance(Floor, EFloor) ->
	D = Floor - EFloor,
	if 
		D > 0 ->
			D;
		true ->
			-D
	end.
