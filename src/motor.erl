-module(motor).



-export([start/1]).

-define(TRAVEL_TIME, 3000). %3seconds


start(ParentPid) ->
	Pid = spawn_link(fun() -> init(ParentPid) end),
	{ok, Pid}.	

init(ParentPid) ->
	standby(ParentPid).


standby(ParentPid) ->
	io:format("Motor ~p. Standby2~n", [self()]),
	receive
		{move, Direction} ->
			io:format("Motor ~p. starting to move in direction ~p~n", [self(), Direction]),
			move(ParentPid, Direction);
		L ->
			io:format("Motor ~p. Received msg: ~p~n", [self(), L]),
			standby(ParentPid)
	end.

move(ParentPid, Direction) ->
	receive
		{cancel, Why} ->
			io:format("Motor ~p was moving. Received cancel for reason: ~p~n", [self(), Why]),
			standby(ParentPid)
	after ?TRAVEL_TIME ->
		ParentPid ! {motor, next_floor, Direction},
		standby(ParentPid)
	end.