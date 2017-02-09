-module(interface_server).
%% gen_server_mini_template
-behaviour(gen_server).
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([register_client/1, unregister_client/1,status_update/1, received_ext_msgs/1]).

-define(SERVER, ?MODULE).

-record(state, {
	tcpClients = sets:new()
	}).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) -> 
	{ok, _} = tcp_interface:start(),
	{ok, #state{}}.

register_client(ClientPid) ->
	gen_server:cast(?SERVER, {register_client, ClientPid}).
unregister_client(ClientPid) ->
	gen_server:cast(?SERVER, {unregister_client, ClientPid}).

status_update(UpdateMessage) ->
	gen_server:cast(?SERVER, {status_update, UpdateMessage}).

received_ext_msgs(Msgs) ->
	% Messages received from client(s)
	lists:foreach(fun(Msg) -> external_message(Msg) end, Msgs).
external_message(Msg) ->
	gen_server:cast(?SERVER, {external_message, Msg}).


handle_call(_Request, _From, State) -> {reply, aReply, State}.


handle_cast({register_client, ClientPid}, State) -> 
	{noreply, State#state{tcpClients=sets:add_element(ClientPid, State#state.tcpClients)}};
handle_cast({unregister_client, ClientPid}, State) -> 
	{noreply, State#state{tcpClients=sets:del_element(ClientPid, State#state.tcpClients)}};
handle_cast({external_message, Msg}, State) -> 
	io:format("External Message: ~p~n", [Msg]),
	MsgList = string:tokens(binary_to_list(Msg), ","),
	handle_external_message(MsgList),
	{noreply, State};
handle_cast({status_update, UpdateMessage}, State) -> 
	send_update(UpdateMessage, State),
	{noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({to_all, M}, State) -> 
	send_to_all(State#state.tcpClients, M),
	{noreply, State};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> 
	ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


send_to_all(Clients, Msg) ->
	ClientsList = sets:to_list(Clients),
	lists:foreach(fun(Cli) -> Cli ! {internal, Msg} end, ClientsList).

send_update(UpdateMessage, State) ->
	case UpdateMessage of
		{elevator, Number, Position, Direction} ->
			send_to_all(State#state.tcpClients, io_lib:format("E,~p,~p,~p",[Number, Position, Direction]) );
		{floor_consumed, Floor, Direction} ->
			send_to_all(State#state.tcpClients, io_lib:format("FC,~p,~p",[Floor, Direction]) )
	end.

handle_external_message(["CALL"|L]) ->
	[Floor, Direction, Status] = L,
	{FloorInt, _} = string:to_integer(Floor),
	if 
		Status =:= "1" ->
			elevator_control_server:user_call(FloorInt, list_to_atom(Direction) );
		true ->
			elevator_control_server:user_call_cancel(FloorInt, list_to_atom(Direction))
	end,
	ok;
handle_external_message(["INTERNAL_BUTTON"|L]) ->
	[Elevator, Floor, Status] = L, 
	{ElevatorNum, _} = string:to_integer(Elevator),
	{FloorNum, _} = string:to_integer(Floor),
	if 
		Status =:= "1" ->
			elevator_control_server:elevator_local_destination(ElevatorNum, FloorNum);
		true ->
			elevator_control_server:elevator_local_destination_cancel(ElevatorNum, FloorNum)
	end,
	ok;
handle_external_message(_L) ->
	ok.
