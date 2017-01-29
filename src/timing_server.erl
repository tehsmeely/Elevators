-module(timing_server).
%% gen_server_mini_template
-behaviour(gen_server).
-export([start_link/0, create_timer/2, update_timer/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-define(SERVER, ?MODULE).


-record(state, {activeTimers=dict:new()}).

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



%% Gen_server callbacks
handle_call({create_timer, {Duration, ReturnMsg}}, {From, _}, State) -> 
	ExtRef = make_ref(),
	Timer = timer:send_after(Duration, {timer_end, ExtRef}),
	Record = #timer{
		externalRef = ExtRef,
		startTime = erlang:timestamp(),
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
	%cancel timer
	timer:cancel(Record#timer.timerRef),
	%add new timer
	NewTimer = timer:send_after(NewDuration, {timer_end, TimerExtRef}),
	io:format("timer server updated timer ~p~n", [NewTimer]),
	UpdatedRecord = Record#timer{timerRef = NewTimer, currentDuration=NewDuration},
	NewActiveTimers = dict:store(TimerExtRef, UpdatedRecord, State#state.activeTimers),
	NewState = State#state{activeTimers=NewActiveTimers},
	{reply, TimerExtRef, NewState};

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