-module(interface_server).
%% gen_server_mini_template
-behaviour(gen_server).
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([register_client/1, unregister_client/1,status_update/1,
	received_ext_msgs/2, send_ready/1]).

-define(SERVER, ?MODULE).

-include("states.hrl").

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

received_ext_msgs(Msgs, From) ->
	% Messages received from client(s)
	lists:foreach(fun(Msg) -> external_message(Msg, From) end, Msgs).
external_message(Msg, From) ->
	gen_server:cast(?SERVER, {external_message, {Msg, From}}).


send_ready(TickID) ->
	gen_server:cast(?SERVER, {status_update, {send_ready, TickID}}).



handle_call(_Request, _From, State) -> {reply, aReply, State}.


handle_cast({register_client, ClientPid}, State) -> 
	{noreply, State#state{tcpClients=sets:add_element(ClientPid, State#state.tcpClients)}};
handle_cast({unregister_client, ClientPid}, State) -> 
	{noreply, State#state{tcpClients=sets:del_element(ClientPid, State#state.tcpClients)}};
handle_cast({external_message, {Msg, From} }, State) -> 
	io:format("External Message: ~p~n", [Msg]),
	MsgList = string:tokens(binary_to_list(Msg), ","),
	io:format("External Message, List: ~p~n", [MsgList]),
	handle_external_message(MsgList, From, State),
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
send_to_all(Clients, Msg, Block) ->
	send_to_all(sets:del_element(Block, Clients), Msg).

send_update(UpdateMessage, State) ->
	case UpdateMessage of
		%{E_NPD}
		{elevator, Number, Position, Direction} ->
			send_to_all(State#state.tcpClients, io_lib:format("E,~p,~p,~p",[Number, Position, Direction]) );

		%{FC_FD}
		{floor_consumed, Floor, Direction} ->
			send_to_all(State#state.tcpClients, io_lib:format("FC,~p,~p",[Floor, Direction]) );

		%{UC_FD}
		{user_call, Floor, Direction} ->
			send_to_all(State#state.tcpClients, io_lib:format("UC,~p,~p",[Floor, Direction]) );

		%{TS_E}
		{temporary_stop, ElevatorNum, Direction} ->
			send_to_all(State#state.tcpClients, io_lib:format("TS,~p,~p",[ElevatorNum, Direction]) );

		%{TSE_E}
		{temporary_stop_end, ElevatorNum} ->
			send_to_all(State#state.tcpClients, io_lib:format("TSE,~p",[ElevatorNum]) );

		%{SR}
		{send_ready, TickID} ->
			send_to_all(State#state.tcpClients, io_lib:format("READY,~s",[TickID]))
	end.

% handle_external_message(["TICK"], _From, _State) ->
% 	%tick the times, then wait for something to happen.
% 	timing_server:tick(),
% 	elevator_control_server:synchronise();
% 	%%send_ready will be called when synchronised

handle_external_message(["TICK"|[TickID]], _From, _State) ->
	%tick the times, then wait for something to happen.
	timing_server:tick(),
	elevator_control_server:synchronise(TickID);
	%%send_ready will be called when synchronised

handle_external_message(["CALL"|L], _From, _State) ->
	[Floor, Direction, Status] = L,
	{FloorInt, _} = string:to_integer(Floor),
	if 
		Status =:= "1" ->
			elevator_control_server:user_call(FloorInt, list_to_atom(Direction) );
		true ->
			elevator_control_server:user_call_cancel(FloorInt, list_to_atom(Direction))
	end;
handle_external_message(["INTERNAL_BUTTON"|L], _From, _State) ->
	[Elevator, Floor, Status] = L, 
	{ElevatorNum, _} = string:to_integer(Elevator),
	{FloorNum, _} = string:to_integer(Floor),
	if 
		Status =:= "1" ->
			elevator_control_server:elevator_local_destination(ElevatorNum, FloorNum);
		true ->
			elevator_control_server:elevator_local_destination_cancel(ElevatorNum, FloorNum)
	end;
handle_external_message(["INIT_ME"|[TickID]], From, _State) ->
	io:format("INIT request received from: ~p~n", [From]),
	lists:foreach(
		fun(Elevator) -> 
			Number = Elevator#elevatorState.number,
			Position = Elevator#elevatorState.floor,
			Direction = Elevator#elevatorState.direction,
			SendString = io_lib:format("E,~p,~p,~p",[Number, Position, Direction]),
			io:format("INIT sending: ~p~n", [SendString]),
			From ! {internal, SendString}
		end,
		elevator_control_server:get_elevators()
	),
	From ! {internal, io_lib:format("READY,~s",[TickID])};

handle_external_message(["REPORT"|_]=M, From, State) ->
	io:format("REPORT ~p received from: ~p~n", [M, From]),
	ReportStr = string:join(M, ","),
	send_to_all(State#state.tcpClients, ReportStr, From);

handle_external_message(["ELEVATORREPORT"|_]=M, From, State) ->
	io:format("ELEVATORREPORT ~p received from: ~p~n", [M, From]),
	ReportStr = string:join(M, ","),
	send_to_all(State#state.tcpClients, ReportStr, From);


handle_external_message(["RESET"], From, State) ->
	%reset whole app
	%elevator supervisor has to kill all children, and ECS remove all knowledge
	elevator_control_server:reset_all(),
	send_to_all(State#state.tcpClients, "RESETCOMPLETE").
