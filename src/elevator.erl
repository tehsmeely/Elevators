-module(elevator).

-export([start/0]).

-record(state, {
	floor=0, %current floor number
	passengers=0,  %count of passengers in lift
	floorDestinations=[], 
	motor
}).

-define(STOP_DELAY, 4000).


start() ->
	Pid = spawn_link(fun() -> init() end),
	{ok, Pid}.	


init() ->
	{ok, Motor} = motor:start(2),
	elevator_control_server:register_elevator(self()),
	stationary(#state{motor=Motor}).


stationary(State) ->
	receive
		{destination, _, D, _} = M ->  %local or ECS destination entry
			io:format("~p: Received destination message ~p~n", [self(), M]),
			if 
				State#state.floor == D ->
					io:format("~p, Floor entered is same as current, staying still~n",[self()]),
					stationary(State);
				true ->
					Direction = destinationDirection(State#state.floor, D),
					io:format("~p, Moving to floor ~p, direction ~p~n",[self(), D, Direction]),
					State#state.motor ! {move, Direction},
					moving(State#state{floorDestinations=[D]}, Direction)
			end;
		Any -> 
			io:format("~p: Received unknown message ~p~n", [self(), Any]),
			stationary(State)
	end. 

moving(State, Direction) ->
	receive
		{motor, next_floor} = M ->
			io:format("~p: Received position update ~p~n", [self(), M]),
			NewFloor = floor_incr(State#state.floor, Direction),
			FloorWaiting = elevator_control_server:check_pending(NewFloor, Direction),
			if
				FloorWaiting ->
					elevator_control_server:stopped_at_floor(NewFloor, Direction),
					temporary_stop(State, Direction)
			end,
			moving(State, Direction);
		{destination, local, D, _} ->
			io:format("~p: Received local destination addition ~p~n", [self(), D]),
			moving(State#state{floorDestinations=lists:sort([D|State#state.floorDestinations])}, Direction);
		Any ->
			io:format("~p: Received unknown message ~p~n", [self(), Any]),
			moving(State, Direction)
	end. 	


temporary_stop(State, Direction) ->
	Ref = timing_server:create_timer(?STOP_DELAY, {lift_move, now}),
	temporary_stop_wait(State, Ref, Direction).

temporary_stop_wait(State, Tref, Direction) ->
	receive
		{passengers, N} = M ->
			io:format("~p: Received passenger adjustment ~p~n", [self(), M]),
			temporary_stop_wait(State#state{passengers=passengers+N}, Tref, Direction);
		{local_button, close} ->
			ok;
		{local_button, open} ->
			timing_server:update_timer(Tref, ?STOP_DELAY),
			temporary_stop_wait(State, Tref, Direction);
		{destination, local, D, _} ->
			io:format("~p: Received local destination addition ~p~n", [self(), D]),
			moving(State, Direction);
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
		Direction == up ->
			Floor+1;
		true ->
			Floor-1
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



