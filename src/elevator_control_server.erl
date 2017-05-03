-module(elevator_control_server).
%% gen_server_mini_template
-behaviour(gen_server).
-export([start_link/0, register_elevator/3,user_call/2,user_call_cancel/2,
	stopped_at_floor/1, elevator_local_destination/2, get_elevators/0,
	elevator_local_destination_cancel/2, synchronise/1, stationary_check/1,
	update_state/2, reset_all/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

-include("states.hrl").

-record(state, {
	elevators = dict:new(),
	elevatorStates = dict:new(),
	pending_up = ordsets:new(),
	pending_down = ordsets:new(),
	ping_response_count = 0,
	currentTickID
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


stationary_check(State) ->
	gen_server:call(?SERVER, {stationary_check, {State}}).

get_elevators() ->
	gen_server:call(?SERVER, {get_elevators}). 


update_state(ElevatorPid, State) ->
	gen_server:cast(?SERVER, {update_state, {ElevatorPid, State} }). 


synchronise(TickID) ->
	%This should do its best to ensure all eleavtors are ready and waiting for a fresh tick
	% special function here in-case it needs extending
	gen_server:cast(?SERVER, {check_ping_all, TickID}). 


reset_all() ->
	gen_server:call(?SERVER, {reset_all}). 

%%% Gen_server Callbacks

handle_call({register_elevator, {Name, Pid, ElevatorState}}, _From, State) ->
	io:format("ECS: Registering elevator with PID ~p~n", [Pid]),
	NewElevators = dict:store(Name, Pid, State#state.elevators),
	NewElevatorStates = dict:store(Pid, ElevatorState, State#state.elevatorStates),
	{reply, ok, State#state{elevators=NewElevators, elevatorStates=NewElevatorStates}};
handle_call({user_call, {Floor, Direction}}, _From, State) ->
	io:format("ECS: User call from floor ~p Direction ~p~n", [Floor, Direction]),
	%{UC_FD}
	interface_server:status_update({user_call, Floor, Direction}),
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
	io:format("dict:store(~p, ~p),~n", [From, ElevatorState]),
	NewStatesDict = dict:store(From, ElevatorState, OldState#state.elevatorStates),
	State = OldState#state{elevatorStates=NewStatesDict},
	if 
		Direction =:= up ->
			Resp = ordsets:is_element(Floor, State#state.pending_up),
			if 
				Resp ->
					NewSet = ordsets:del_element(Floor, State#state.pending_up),
					%interface_server:status_update({floor_consumed, Floor, Direction}),
					NewState = State#state{pending_up=NewSet};
				true ->
					NewState = State
			end;
		true ->
			Resp = ordsets:is_element(Floor, State#state.pending_down),
			if 
				Resp ->
					NewSet = ordsets:del_element(Floor, State#state.pending_down),
					%interface_server:status_update({floor_consumed, Floor, Direction}),
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
handle_call({get_elevators}, _From, State) -> 
	{reply, helper_lib:get_values(State#state.elevatorStates), State};

handle_call({stationary_check, {ElevatorState}}, _, State) -> 
	Reply = check_floors_pending(State, ElevatorState),
	{reply, Reply, State};

handle_call(reset_all, _From, State) -> 
	%Wipe state, and restart all elevators
	lists:foreach(fun(Elevator) ->
			Elevator ! restart
		end,
		dict:fetch_keys(State#state.elevatorStates)
	),
	timing_server:clear_all(),
	NewState = #state{},
	{reply, ok, NewState};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({check_ping_all, TickID}, State) -> 
	%Ping all the elevators. They will respond when idling so we know it's ready to continue
	%clear the count now so on receipt of each message it can be checked!
	lists:foreach(fun(Elevator) ->
			Elevator ! {check_ping, self(), TickID}
		end,
		dict:fetch_keys(State#state.elevatorStates)
	),
	{noreply, State#state{ping_response_count=0, currentTickID=TickID}};

handle_cast({update_state, {Pid, ElevatorState}}, State) ->
	OldStates = State#state.elevatorStates,
	NewStates = dict:store(Pid, ElevatorState, OldStates),
	{noreply, State#state{elevatorStates=NewStates}};

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({elevator_ok, _ElevatorPid, TickID}, State) -> 
	%Each elevator should reply with this message, once they all have we can tell the client to continue
	if 
		TickID =:= State#state.currentTickID ->
			NewCount = State#state.ping_response_count + 1,
			DictSize = dict:size(State#state.elevators),
			if
				NewCount == DictSize ->
					io:format("ECS Received elevator_ok message. count == numElevators, sending READY~n",[]),
					interface_server:send_ready(TickID);
				NewCount > DictSize ->
					io:format("ECS Received elevator_ok message when count is > num elevators, did it not get cleared?~n",[]);
				true ->
					io:format("ECS Received elevator_ok message. count < numElevators: ~p~n",[NewCount]),
					ok
			end;
		 true ->
		 	io:format("ECS Received elevator_ok message with invalid tick ID: ~p~n",[TickID]),
		 	NewCount = State#state.ping_response_count
		 end,
	{noreply, State#state{ping_response_count=NewCount}};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% Internal Functions

broadcast_call(State, Floor, Direction) ->
	%Need to pick the best elevator., but if one isnt available jsut leave it

	io:format("Closest valid for ~p ~p~n", [Floor, Direction]),

	case 
		get_closest_valid(dict:to_list(State#state.elevatorStates), {none, -1}, {Floor, Direction})
		 	of
		 {ok, {ElevatorPid, _}} ->
		 	io:format("Found: ~p~n", [ElevatorPid]),
		 	ElevatorPid ! {destination, global, Floor, Direction};
		 {not_found} ->
		 	io:format("Not Found!~n", []),
		 	ok
	end.

get_closest_valid([], {_ClosestElevator, -1}, _CallDetails) ->
	{not_found};
get_closest_valid([], {ClosestElevator, _}, _CallDetails) ->
	{ok, ClosestElevator};	
get_closest_valid([{Pid, ElevatorState}|L], {ClosestElevator, ClosestDistance}, {Floor, Direction}) ->
	%% IF direction of elevator matches button direction or is none:
	%	check the distance and replace if distance is shorter then loop
	%io:format("ecs:get_closest_valid([{~p, ~p}|~p], {~p, ~p}, {~p, ~p})~n", [Pid, ElevatorState,L , ClosestElevator, ClosestDistance, Floor, Direction]),
	io:format("ecs:get_closest_valid. Current Closest Dist: ~p. For Floor:~p, Direction: ~p~n Elevator state for this loop:~n", [ClosestDistance, Floor, Direction]),
	print_elevator_state(ElevatorState),

	Distance = get_distance(Floor, ElevatorState#elevatorState.floor),
	if ClosestDistance == -1 ->
		io:format("No distance set, just picking if is None~n"),
		if ElevatorState#elevatorState.direction =:= none ->
			io:format("Picking this and looping, Dist: ~p~n",[Distance]),
			get_closest_valid(L, {{Pid, ElevatorState}, Distance}, {Floor, Direction});
		true ->
			% move on
			io:format("Not none, ignoring and looping~n"),
			get_closest_valid(L, {ClosestElevator, ClosestDistance}, {Floor, Direction})
		end;
	true ->
		io:format("Distance is set, Only picking if None and distance is less, this dist: ~p~n",[Distance]),
		case int_comp(ElevatorState#elevatorState.direction, none) of
			true when Distance < ClosestDistance ->
				% is best
				io:format("Picking this and looping~n"),
				get_closest_valid(L, {{Pid, ElevatorState}, Distance}, {Floor, Direction});
			_ ->
				% move on
				io:format("Not none or dist is higher, ignoring and looping~n"),
				get_closest_valid(L, {ClosestElevator, ClosestDistance}, {Floor, Direction})
		end
	end.



check_floors_pending(State, ElevatorState) ->
	if 
		length(State#state.pending_up) > 0; length(State#state.pending_down) > 0 ->
			Floor = get_closest_of(
				ordsets:to_list(ordsets:union(State#state.pending_down, State#state.pending_up)), 
				ElevatorState#elevatorState.floor
			),
			{true, Floor};
		true ->
			{false, -1}
	end.

get_closest_of([First|List], Point) ->
	get_closest_of(List, {First, abs(First-Point)}, Point).

get_closest_of([], {Closest, _ClosestDistance}, _Point) ->
	Closest;
get_closest_of([D|L], {Closest, ClosestDistance}, Point) ->
	Distance = abs(D - Point),
	if 
		Distance < ClosestDistance ->
			get_closest_of(L, {D, Distance}, Point);
		true ->
			get_closest_of(L, {Closest, ClosestDistance}, Point)
	end.

int_comp(A, B) -> A=:=B.
get_distance(Floor, EFloor) ->
	D = Floor - EFloor,
	if 
		D > 0 ->
			D;
		true ->
			-D
	end.



print_elevator_state(State) ->
	io:format("Elevator ~p, state:~nfloor: ~p~npassengers: ~p~nfloorDestinations: ~p~ndirection: ~p~nmotor: ~p~nparent: ~p~n",
		[
			State#elevatorState.number,
			State#elevatorState.floor,
			State#elevatorState.passengers,
			State#elevatorState.floorDestinations,
			State#elevatorState.direction,
			State#elevatorState.motor,
			State#elevatorState.parent
		]
	).