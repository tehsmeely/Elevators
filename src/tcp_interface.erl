-module(tcp_interface).

-define(PORT, 36000).

-export([start/0]).

start() ->
	{ok, Listen} = gen_tcp:listen(?PORT, [{active, once}]),
	Pid = spawn(fun() -> parallel_connection_listen(Listen) end),
	{ok, Pid}.


parallel_connection_listen(Listen) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	spawn(fun() -> parallel_connection_listen(Listen) end),
	interface_server:register_client(self()),
	server_loop(Socket).

server_loop(Socket) ->
	receive
		{internal, Msg} -> 
			%io:format("Internal msg received: ~p~n", [M]),
			gen_tcp:send(Socket, netstring:encode(Msg)),
			%io:format("send: ~p~n", [S]),
			server_loop(Socket);
		{tcp, Socket, Data}=M ->
			io:format("External msg received: ~p~n", [M]),
			Msgs = decode_multi(list_to_binary(Data)),
			io:format("DecodedMsg: ~p~n", [Msgs]),
			interface_server:received_ext_msgs(Msgs, self()),
			%reenable active_once
			inet:setopts(Socket, [{active, once}]),
			server_loop(Socket);
		{tcp_closed, Socket} ->
			io:format("TCP closed~n", []),
			interface_server:unregister_client(self()),
			ok
	end.


decode_multi(NetStr) -> lists:reverse(decode_multi([],netstring:decode(NetStr))).
decode_multi(L, {Data,<<>>}) -> [Data|L];
decode_multi(L, {Data,Extra}) -> decode_multi([Data|L], netstring:decode(Extra)).
