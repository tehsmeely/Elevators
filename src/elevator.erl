-module(elevator).

-export([start/1]).

-export([end_of_direction/3]).

% -record(elevatorState, {
% 	floor=0, %current floor number
% 	passengers=0,  %count of passengers in lift
% 	floorDestinations=[],
% 	direction,
% 	motor,
% 	number
% }).
-include("states.hrl").

-define(STOP_DELAY, 4000).


start(N) ->
	Pid = spawn_link(fun() -> init(N) end),
	{ok, Pid}.	


init(Number) ->
	{ok, Motor} = motor:start(self()),
	State = #elevatorState{motor=Motor, direction=none, number=Number},
	elevator_control_server:register_elevator(Number, self(), State),
	stationary(State).


stationary(State) ->
	io:format("Elevator ~p: Stationary ~p~n", [self(), State]),
	receive
		{destination, _, D, _} = M ->  %local or ECS destination entry
			io:format("Elevator ~p: Received destination message ~p~n", [self(), M]),
			if 
				State#elevatorState.floor =:= D ->
					io:format("Elevator ~p, Floor entered is same as current, staying still~n",[self()]),
					stationary(State);
				true ->
					Direction = destinationDirection(State#elevatorState.floor, D),
					io:format("Elevator ~p, Moving to floor ~p, direction ~p~n",[self(), D, Direction]),
					State#elevatorState.motor ! {move, Direction},
					moving(State#elevatorState{floorDestinations=[D], direction=Direction})
			end;
		Any -> 
			io:format("Elevator ~p: Received unknown message ~p~n", [self(), Any]),
			stationary(State)
	end. 

moving(State) ->
	io:format("Elevator ~p: Moving ~p~n", [self(), State]),
	receive
		{motor, next_floor, _} = M ->
			io:format("Elevator ~p: Received position update ~p~n", [self(), M]),
			NewFloor = floor_incr(State#elevatorState.floor, State#elevatorState.direction),
			{UpdState, Behaviour} = check_floor_behaviour(State, NewFloor),
			io:format("Elevator ~p: Behaviour following will be: ~p~n", [self(), Behaviour]),
			interface_server:status_update({elevator, UpdState#elevatorState.number, NewFloor, UpdState#elevatorState.direction}),
			case Behaviour of
				local_end ->
					temporary_stop(UpdState#elevatorState{direction=none}, stop);
					%stationary();
				local_end_reverse ->
					temporary_stop(UpdState#elevatorState{direction=invert_direction(UpdState#elevatorState.direction)}, move);
					% UpdState#elevatorState.motor ! {move, UpdState#elevatorState.direction},
					% moving(UpdState);
				temp_stop ->
					temporary_stop(UpdState, move);
					% UpdState#elevatorState.motor ! {move, UpdState#elevatorState.direction},
					% moving(UpdState);
				dont_stop ->		
					io:format("Elevator ~p: Resignalling motor to continue ~p~n", [self(), State#elevatorState.direction]),
					State#elevatorState.motor ! {move, State#elevatorState.direction},
					moving(UpdState)
			end;
		{destination, _, D, _} ->
			io:format("Elevator ~p: Received local destination addition ~p~n", [self(), D]),
			moving(State#elevatorState{floorDestinations=lists:sort([D|State#elevatorState.floorDestinations])});
		Any ->
			io:format("Elevator ~p: Received unknown message ~p~n", [self(), Any]),
			moving(State)
	end. 	


temporary_stop(State, EndMode) ->
	interface_server:status_update({temporary_stop, State#elevatorState.number}),
	Ref = timing_server:create_timer(?STOP_DELAY, {lift_move, now}),
	temporary_stop_wait(State, Ref, EndMode).

temporary_stop_wait(State, Tref, EndMode) ->
	receive
		{passengers, N} = M ->
			io:format("Elevator ~p: Received passenger adjustment ~p~n", [self(), M]),
			temporary_stop_wait(State#elevatorState{passengers=passengers+N}, Tref, EndMode);
		{local_button, close} ->
			ok;
		{local_button, open} ->
			timing_server:update_timer(Tref, ?STOP_DELAY),
			temporary_stop_wait(State, Tref, EndMode);
		{destination, local, D, _} ->
			io:format("Elevator ~p: Received local destination addition ~p~n", [self(), D]),
			temporary_stop_wait(State#elevatorState{floorDestinations=lists:sort([D|State#elevatorState.floorDestinations])}, Tref, EndMode);
		{lift_move, now} ->
			ok
	end,
	interface_server:status_update({temporary_stop_end, State#elevatorState.number}),
	case EndMode of
		move ->
			State#elevatorState.motor ! {move, State#elevatorState.direction},
			moving(State);
		stop ->
			stationary(State)
	end.



%%% INTERNAL FUNCTIONS
destinationDirection(Current, Dest) ->
	if 
		Current > Dest ->
			down;
		true ->
			up
	end.

floor_incr(Floor, Direction) ->
	if 
		Direction =:= up ->
			Floor+1;
		true ->
			Floor-1
	end.

check_floor_behaviour(OldState, NewFloor) ->
%%Arriving at a floor:
% a) local_end: is a local destination and last in sequence - go stationary
% b) local_end_reverse: is a local destination and last in sequence but alternate directions present - temporary stop, change dir
% c) temp_stop: is a local destination and not last in sequence - temporary stop
% d) temp_stop: is not a local destination but has pending in same direction - temporary stop
% e) dont_stop: is not a local destination and not pending - keep going!
	State = OldState#elevatorState{floor=NewFloor},
	ECSResponse = elevator_control_server:stopped_at_floor(State),
	case lists:member(NewFloor, State#elevatorState.floorDestinations) of
		true ->
		%% Local Destination
			NewDestinations = lists:delete(NewFloor, State#elevatorState.floorDestinations),
			case length(NewDestinations) of
				0 ->
					RetVal = local_end;  %a
				_ ->
					case end_of_direction(NewFloor, NewDestinations, State#elevatorState.direction) of
						true ->
							RetVal = temp_stop;  %c
						_ ->
							RetVal = local_end_reverse %b
					end
			end,
			NewState = State#elevatorState{floorDestinations=NewDestinations};
		false ->
		%% Is not Local Destination
			case ECSResponse of
				true ->
					RetVal = temp_stop;
				_ ->
					RetVal = dont_stop %
			end,
			NewState = State
	end,
	{NewState, RetVal}.


end_of_direction(Floor, Destinations, Direction) ->
	%use direction to ascertain if NewFloor is end of current direction in floorDestinations
	case Direction of
		up -> %NewFloor is end if greater than max of remaining
			Floor > lists:max(Destinations);
		down -> %NewFloor is enf if less than min of remaining
			Floor < lists:min(Destinations)
	end.


invert_direction(Direction) ->
	case Direction of
		up -> down;
		down -> up
	end.


%%  States
%%------------------
%
%  Stationary
% ============
% At Floor N
% Can receive or unload passengers
% Can receive local floor commands (Users) or remote floor commands (ECS)
%
%  Moving
% ========
% Moving from floor to floor
% Checks each floor if it is waiting
% Can receive local floor commands (Users) only
% Receives location updates from Motor

%  Temporary Stop
% ================
% Stopping but he have passengers and know where to go next

