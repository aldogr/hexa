-module(state_machine).

-behaviour(gen_fsm).

-export([start_link/0]).

-export([init/1, first_state/2, first_state/3, second_state/3, second_state/2,
	 third_state/2, third_state/3, auto/2, auto/3, manual/2, manual/3,
 	handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


-export([next/0, reset/0, sync_reset/0, land/0, sync_land/0]).

-define(SERVER, ?MODULE).

-include("../include/remote.hrl").
-include("../include/imu.hrl").
-include("../include/pwm.hrl").
-include("../include/controller.hrl").


-record(state, {times}).

start_link() ->
    gen_fsm:start_link({global, "MachineName"}, ?MODULE, [], []).



next() ->    gen_fsm:sync_send_event({global, "MachineName"}, next).
%Synchronous


reset() ->
    gen_fsm:send_all_state_event({global, "MachineName"}, reset).

land() ->
    gen_fsm:send_all_state_event({global, "MachineName"}, land).

sync_reset() ->
    gen_fsm:sync_send_all_state_event({global, "MachineName"}, reset).

sync_land() ->
    gen_fsm:sync_send_all_state_event({global, "MachineName"}, land).




init([]) ->
	%St = 0,   %%reset state of the motors and motors
		%times = 0,
    {ok, first_state, #state{}}.

%-----------------------------------------------------------------------------------
% Async events.
%-----------------------------------------------------------------------------------
first_state(next, #state{} = State) ->
    io:format("Ready to calibrate~n"),

	
%%%commads go here
	gen_server:cast(pwm,{setState, out}),

%%%%
%%%%out for not having the motors at 0.4, waiting for another state to be necessary
%%%%

    {next_state, second_state, State#state{times = 0}}.

second_state(next, #state{times = N} = State) ->
    io:format("Calibrating...~n"),

	gen_server:cast(pwm,{setState, out}),

    {next_state, third_state,State#state{times = N+1}}.

third_state(next, #state{times = N} = State) ->
    io:format("Arming...~n"),
	gen_server:cast(pwm,{setState, stop}),
	%%%%stops the motors again -->>> check if necessary
    {next_state, auto, State#state{times = N+1}}. 


auto(next, #state{} = State) ->
    io:format("Auto Mode~n"),
	gen_server:cast(pwm,{setState, flying}),
    {next_state, manual, State#state{times = 3}}.

manual(next, #state{} = State) ->
    io:format("Manual Mode~n"),
	gen_server:cast(pwm,{setState, flying}),
    {next_state, auto, State#state{times = 4}}.



%-----------------------------------------------------------------------------------
% Sync events.
%-----------------------------------------------------------------------------------
first_state(Event, _From, State) ->
    {next_state, NextState, NewState} = first_state(Event, State),
    {reply, NewState#state.times, NextState, NewState}.

second_state(Event, _From, State) ->
    {next_state, NextState, NewState} = second_state(Event, State),
    {reply, NewState#state.times, NextState, NewState}.

third_state(Event, _From, State) ->
    {next_state, NextState, NewState} = third_state(Event, State),
    {reply, NewState#state.times, NextState, NewState}.

auto(Event, _From, State) ->
    {next_state, NextState, NewState} = auto(Event, State),
    {reply, NewState#state.times, NextState, NewState}.

manual(Event, _From, State) ->
    {next_state, NextState, NewState} = manual(Event, State),
    {reply, NewState#state.times, NextState, NewState}.



%-----------------------------------------------------------------------------------
% Async events (All).
%-----------------------------------------------------------------------------------
handle_event(reset, _StateName, _State) ->  
    io:format("Async Reset (STOP)...~n"), 
  	gen_server:cast(pwm,{setState, stop}),
    {next_state, first_state, #state{}};

handle_event(land, _StateName, _State) ->
    io:format("Async Landing...~n"),
	%%%commands here
	gen_server:cast(pwm,{setState, stop}),
    {next_state, first_state, #state{}}.

%-----------------------------------------------------------------------------------
% Sync events (All).
%-----------------------------------------------------------------------------------
handle_sync_event(reset, _From, _StateName, _State) ->
    io:format("Sync reset (STOP)...~n"),
	gen_server:cast(pwm,{setState, stop}),
    {reply, 0, first_state, #state{}};

handle_sync_event(land, _From, _StateName, _State) ->
    io:format("Sync Landing...~n"),
	%%%commands here
	gen_server:cast(pwm,{setState, stop}),
    {reply, 0, first_state, #state{}}.

%-----------------------------------------------------------------------------------
% Regular OTP messages.
%-----------------------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
