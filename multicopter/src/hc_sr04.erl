-module(hc_sr04).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../include/sonar.hrl").


-define(Offset, 800).      
-define(Multiplier, 0.01717). 
-define(Trigger_GPIO, 23). %%%%%% check the GPIOs available
-define(Echo_GPIO, 24).    

%%-record(pin, {trg_try :: pin,ech_try}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The initialisation of both GPIOs should be done in init(),
%% and its termination in terminate(),
%% but unfortunately the record does not carry the pids properly
%% and meanwhile the pid are started and stoped in the function.
%% This should be a temporary solution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	io:format("~p starting~n", [?MODULE]),
	process_flag(trap_exit, true),

	%TRG = gpio:init(?Trigger_GPIO, out),
	%ECH = gpio:init(?Echo_GPIO, in),
	%io:format("TRG: ~p~n",[TRG]),
	%#pin{trg_try=TRG, ech_try=ECH},
	%gpio:write(TRG, 0), %works with TRG!

	St = #sonar{
		height = 0.0
	},
	{ok, St}.

handle_call(update_height, _From, State) ->
	case get_height() of
		{ok, Data} -> {reply, Data, Data};
		{error, io, _V} -> {reply, State, State}
	end;
	
	

handle_call(get_height, _From, State) ->
	case State of
		false -> {ok, Data} = get_height();
		State -> Data = State
	end,
	{reply, Data, Data}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	%%gpio:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Internal Functions	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% sends the pulse for the sonar, and receives the echo, and the duration, calculationg the distance
get_height() ->
	TRG = gpio:init(?Trigger_GPIO, out), %%%this should be in init()
	ECH = gpio:init(?Echo_GPIO, in),
	gpio:write(TRG,1),
	timer:sleep(1),
	gpio:write(TRG,0),
	
	Timing=os:timestamp(),
	
	{K,Tdiff}=readEcho(Timing, ECH),
	X=(Tdiff-?Offset)*?Multiplier,

	gpio:stop(TRG),
	gpio:stop(ECH), %%% this should be in terminate
{ok, #sonar{height=X}}.
	
%% reads the echo duration	
readEcho(Time, Epin) ->
	R=gpio:read(Epin),
	Ta = os:timestamp(),
	if 
		R == "1" ->
			while(Ta,Epin);
		true -> readEcho(Time, Epin)
	end,
	Td = timer:now_diff(os:timestamp(), Ta),
{ok, Td}.

while(Time, Epin)->
	R=gpio:read(Epin),
	if
		R =="0"->D=1;
		true -> while(Time, Epin)
	end,	
{ok}.