-module(motor).



-export([start/1, kill/1]).

%-define(TRAVEL_TIME, 1500). %3seconds
-define(TRAVEL_TIME, 2). % 2 ticks = 14s

-define(KILL_TIMEOUT, 1000);

start(ParentPid) ->
	Pid = spawn_link(fun() -> init(ParentPid) end),
	{ok, Pid}.	

init(ParentPid) ->
	standby(ParentPid).


kill(MotorPid) ->
	%%%External function to kill a specific motor. Sends message and waits for response
	% will kill after timeout if motor doesnt respond
	MotorPid ! close;
	receive 
		{MotorPid, ok} ->
			ok
	after 
		?KILL_TIMEOUT ->
			exit(MotorPid, "Gentle kill timeout"),
			ok
	end.




standby(ParentPid) ->
	io:format("Motor ~p. Standing by~n", [self()]),
	receive
		{move, Direction} ->
			io:format("Motor ~p. starting to move in direction ~p~n", [self(), Direction]),
			move(ParentPid, Direction);
		{close, Killer} ->
			Killer ! {self(), ok},
			ok;
		L ->
			io:format("Motor ~p. Received msg: ~p~n", [self(), L]),
			standby(ParentPid)
	end.

move(ParentPid, Direction) ->
	Ref = timing_server:create_timer(?TRAVEL_TIME, {motor_finish, now}),
	io:format("Motor ~p. Moving ~p~n", [self(), Direction]),
	receive
		{cancel, Why} ->
			io:format("Motor ~p was moving. Received cancel for reason: ~p~n", [self(), Why]),
			timing_server:cancel_timer(Ref),
			standby(ParentPid);
		{close, Killer} ->
			timing_server:cancel_timer(Ref),
			Killer ! {self(), ok},
			ok;
		{motor_finish, now} ->
			ParentPid ! {motor, next_floor, Direction},
			standby(ParentPid)
	end.

% move(ParentPid, Direction) ->
% 	io:format("Motor ~p. Moving ~p~n", [self(), Direction]),
% 	receive
% 		{cancel, Why} ->
% 			io:format("Motor ~p was moving. Received cancel for reason: ~p~n", [self(), Why]),
% 			standby(ParentPid)
% 	after ?TRAVEL_TIME ->
% 		ParentPid ! {motor, next_floor, Direction},
% 		standby(ParentPid)
% 	end.