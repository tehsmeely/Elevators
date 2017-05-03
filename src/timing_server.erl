-module(timing_server).
%% gen_server_mini_template
-behaviour(gen_server).
-export([start_link/0, create_timer/2, update_timer/2, cancel_timer/1, tick/0, clear_all/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-define(SERVER, ?MODULE).


-define(MODE, tick).


-record(state, {activeTimers=dict:new(), currentTick=0}).

-record(timer, {
		externalRef,
		startTime,
		initialDuration,
		currentDuration,
		returnMsg,
		parentPid,
		timerRef
	}).


start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
init([]) -> 
	{ok, #state{}}.


%% Exposed Functions
create_timer(Duration, ReturnMsg) ->
	gen_server:call(?SERVER, {create_timer, {Duration, ReturnMsg}}).

update_timer(TimerRef, NewDuration) ->
	gen_server:call(?SERVER, {update_timer, {TimerRef, NewDuration}}).

cancel_timer(TimerRef) ->
	gen_server:call(?SERVER, {cancel_timer, {TimerRef}}).

tick() ->
	gen_server:call(?SERVER, {tick}).

clear_all() ->
	gen_server:call(?SERVER, {clear_all}).


%% Gen_server callbacks
handle_call({create_timer, {Duration, ReturnMsg}}, {From, _}, State) -> 
	ExtRef = make_ref(),
	case ?MODE of
		tick ->
			Timer = Duration,
			StartTime = State#state.currentTick;
		time ->
			Timer = timer:send_after(Duration, {timer_end, ExtRef}),
			StartTime = erlang:timestamp()
	end,
	Record = #timer{
		externalRef = ExtRef,
		startTime = StartTime,
		initialDuration = Duration,
		currentDuration = Duration,
		returnMsg = ReturnMsg,
		parentPid = From,
		timerRef = Timer
	},
	io:format("timer server created timer ~p~n", [Timer]),
	NewActiveTimers = dict:store(ExtRef, Record, State#state.activeTimers),
	NewState = State#state{activeTimers=NewActiveTimers},
	{reply, ExtRef, NewState};

handle_call({update_timer, {TimerExtRef, NewDuration}}, {_From, _} , State) ->
	Record = dict:fetch(TimerExtRef, State#state.activeTimers),
	case ?MODE of
		tick ->
			NewTimer = NewDuration,
			NewStartTime = State#state.currentTick;
		time ->
			%cancel timer and add new timer	
			timer:cancel(Record#timer.timerRef),
			NewTimer = timer:send_after(NewDuration, {timer_end, TimerExtRef}),
			NewStartTime = erlang:timestamp()
	end,
	io:format("timer server updated timer ~p~n", [NewTimer]),
	UpdatedRecord = Record#timer{timerRef = NewTimer, currentDuration=NewDuration, startTime=NewStartTime},
	NewActiveTimers = dict:store(TimerExtRef, UpdatedRecord, State#state.activeTimers),
	NewState = State#state{activeTimers=NewActiveTimers},
	{reply, TimerExtRef, NewState};

handle_call({cancel_timer, {TimerExtRef}}, {_From, _} , State) ->
	Record = dict:fetch(TimerExtRef, State#state.activeTimers),
	case ?MODE of
		tick ->
			pass;
		time ->
			%cancel timer and add new timer	
			timer:cancel(Record#timer.timerRef)
	end,
	NewActiveTimers = dict:erase(TimerExtRef, State#state.activeTimers),
	NewState = State#state{activeTimers=NewActiveTimers},
	{reply, TimerExtRef, NewState};

handle_call({tick}, _From, State) -> 
	%% Ticking, increment all timers and expire them if after correct duration
	NewCurrentTick = State#state.currentTick+1,
	io:format("TICK: ~p~n",[NewCurrentTick]),
	lists:foreach(
		fun({Ref, TimerRecord}) ->
			ExpiryTime = TimerRecord#timer.startTime + TimerRecord#timer.currentDuration,
			if 
				ExpiryTime =< NewCurrentTick ->
					self() ! {timer_end, Ref};
				true ->
					ok
			end
		end,
		dict:to_list(State#state.activeTimers)
	),
	{reply, ok, State#state{currentTick=NewCurrentTick}};

handle_call(clear_all, _From, State) ->
	%clear the whole timer server down, cancel all timers (if using time) and bin record of them
	case ?MODE of
		tick ->
			pass;
		time ->
			%cancel timer and add new timer
			lists:foreach(
				fun({_Ref, TimerRecord}) ->
					timer:cancel(TimerRecord#timer.timerRef)
				end,
				dict:to_list(State#state.activeTimers)
			)						
	end,
	{reply, ok, #state{}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({timer_end, TimerExtRef}, State) -> 
	%send message to parent and then remove from active dict
	Record = dict:fetch(TimerExtRef, State#state.activeTimers),
	Record#timer.parentPid ! Record#timer.returnMsg,
	NewActiveTimers = dict:erase(TimerExtRef, State#state.activeTimers),
	NewState = State#state{activeTimers=NewActiveTimers},
	{noreply, NewState};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.