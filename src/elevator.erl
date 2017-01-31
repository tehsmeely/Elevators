-module(elevator).

-export([start/0]).

-export([end_of_direction/3]).

-record(state, {
	floor=0, %current floor number
	passengers=0,  %count of passengers in lift
	floorDestinations=[],
	direction,
	motor
}).

-define(STOP_DELAY, 4000).


start() ->
	Pid = spawn_link(fun() -> init() end),
	{ok, Pid}.	


init() ->
	{ok, Motor} = motor:start(self()),
	elevator_control_server:register_elevator(self()),
	stationary(#state{motor=Motor, direction=none}).


stationary(State) ->
	receive
		{destination, _, D, _} = M ->  %local or ECS destination entry
			io:format("Elevator ~p: Received destination message ~p~n", [self(), M]),
			if 
				State#state.floor =:= D ->
					io:format("Elevator ~p, Floor entered is same as current, staying still~n",[self()]),
					stationary(State);
				true ->
					Direction = destinationDirection(State#state.floor, D),
					io:format("Elevator ~p, Moving to floor ~p, direction ~p~n",[self(), D, Direction]),
					State#state.motor ! {move, Direction},
					moving(State#state{floorDestinations=[D], direction=Direction})
			end;
		Any -> 
			io:format("Elevator ~p: Received unknown message ~p~n", [self(), Any]),
			stationary(State)
	end. 

moving(State) ->
%NOTE: Moving also needs to consider local floor queue!!

	receive
		{motor, next_floor, _} = M ->
			io:format("Elevator ~p: Received position update ~p~n", [self(), M]),
			NewFloor = floor_incr(State#state.floor, State#state.direction),
			{UpdState, Behaviour} = check_floor_behaviour(State, NewFloor),
			case Behaviour of
				local_end ->
					stationary(UpdState#state{direction=none});
				local_end_reverse ->
					temporary_stop(UpdState#state{direction=invert_direction(UpdState#state.direction)});
				temp_stop ->
					temporary_stop(UpdState);
				dont_stop ->		
					io:format("Elevator ~p: Resignalling motor to continue ~p~n", [self(), State#state.direction]),
					State#state.motor ! {move, State#state.direction},
					moving(UpdState)
			end;
		{destination, local, D, _} ->
			io:format("Elevator ~p: Received local destination addition ~p~n", [self(), D]),
			moving(State#state{floorDestinations=lists:sort([D|State#state.floorDestinations])});
		Any ->
			io:format("Elevator ~p: Received unknown message ~p~n", [self(), Any]),
			moving(State)
	end. 	


temporary_stop(State) ->
	Ref = timing_server:create_timer(?STOP_DELAY, {lift_move, now}),
	temporary_stop_wait(State, Ref).

temporary_stop_wait(State, Tref) ->
	receive
		{passengers, N} = M ->
			io:format("Elevator ~p: Received passenger adjustment ~p~n", [self(), M]),
			temporary_stop_wait(State#state{passengers=passengers+N}, Tref);
		{local_button, close} ->
			ok;
		{local_button, open} ->
			timing_server:update_timer(Tref, ?STOP_DELAY),
			temporary_stop_wait(State, Tref);
		{destination, local, D, _} ->
			io:format("Elevator ~p: Received local destination addition ~p~n", [self(), D]),
			moving(State);
		{lift_move, now} ->
			ok
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

check_floor_behaviour(State, NewFloor) ->
%%Arriving at a floor:
% a) local_end: is a local destination and last in sequence - go stationary
% b) local_end_reverse: is a local destination and last in sequence but alternate directions present - temporary stop, change dir
% c) temp_stop: is a local destination and not last in sequence - temporary stop
% d) temp_stop: is not a local destination but has pending in same direction - temporary stop
% e) dont_stop: is not a local destination and not pending - keep going!
	case lists:member(NewFloor, State#state.floorDestinations) of
		true ->
		%% Local Destination
			NewDestinations = lists:delete(NewFloor, State#state.floorDestinations),
			case length(NewDestinations) of
				0 ->
					RetVal = local_end;  %a
				_ ->
					case end_of_direction(NewFloor, NewDestinations, State#state.direction) of
						true ->
							RetVal = temp_stop;  %c
						_ ->
							RetVal = local_end_reverse %b
					end
			end,
			NewState = State#state{floorDestinations=NewDestinations};
		false ->
		%% Is not Local Destination
			case elevator_control_server:check_pending(NewFloor, State#state.direction) of
				true ->
					elevator_control_server:stopped_at_floor(NewFloor, State#state.direction),
					RetVal = temp_stop;
				_ ->
					RetVal = dont_stop %
			end,
			NewState = State
	end,
	{NewState#state{floor=NewFloor}, RetVal}.


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

