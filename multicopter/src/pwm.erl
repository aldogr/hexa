-module(pwm).
-behaviour(gen_server).

-export([start_link/0
]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
%% API
-export([getMotor/0]).

-include("../include/pwm.hrl").

getMotor() ->
	gen_server:call(?MODULE, getMotor).


frequency() -> 400.

motors() -> [{a, 0, 0}, {b, 0, 1}, {c, 2, 0},
			{d, 2, 1}, {e, 4, 0}, {f, 4, 1}].

motor(a) -> {0, 0};
motor(b) -> {0, 1};
motor(c) -> {2, 0};
motor(d) -> {2, 1};
motor(e) -> {4, 0};
motor(f) -> {4, 1}.

prefix() -> "/sys/class/pwm/pwmchip".

conf(test_timeout) ->
	{ok, Val} = application:get_env(pwm_test_timeout),
	Val;

conf(test_throttle) ->
	{ok, Val} = application:get_env(pwm_test_throttle),
	Val;

conf(stop_throttle) ->
	{ok, Val} = application:get_env(pwm_stop_throttle),
	Val.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%% @TODO: Also execute enable_all, check if setup scripts were executed.
init([]) ->
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
	enable_all(),
	St = #motorstate{motors = #motorconf{a = 0, b = 0, c = 0, d = 0, e = 0, f = 0},
		st = stop},
    {ok, St}.

handle_call(test, _From, State) ->
	case State#motorstate.st of
		flying ->
			[testmotor(Id) || {Id, _, _} <- motors()],
			{reply, ok, State};
		stop ->
			{reply, {error, stopped}, State}
	end;

handle_call(getMotors, _From, State) ->
	{reply, State#motorstate.motors, State};
	
handle_call(_Msg, _From, State) ->
	{reply, false, State}.

handle_cast({setAllMotors, Conf}, State) ->
	ok = setAllMotors(Conf, State#motorstate.st),
	NewState = State#motorstate{motors = Conf},
	{noreply, NewState};

handle_cast({setState, TargetSt}, State) ->
	NewState = State#motorstate{st = TargetSt},
	{noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions


enable(Chip, Output) ->
	file:write_file(prefix()++integer_to_list(Chip)++"/pwm"++
	integer_to_list(Output)++"/enable", "1").

enable_all() ->
	io:format("Set motor duty to 0~n"),
	[setMotor(Id, 0) || {Id, _, _} <- motors()],
	io:format("Enable all motors~n"),
	[enable(Chip, Output) || {_Motor, Chip, Output} <- motors()],
	ok.

setMotor(Id, Percentage) when
		Percentage >= 0,
		Percentage =< 1 ->
	Duty = float_to_list(Percentage * 1.0e9 / frequency(), [{decimals ,0}]),
	{Chip, Output} = motor(Id),
	file:write_file(prefix()++integer_to_list(Chip)++"/pwm"++
		integer_to_list(Output)++"/duty_cycle", Duty),
	{Id, ok}.


%% @doc Updates all motors
%% @param Conf Record motorconifg, Values between 0..1

-spec setAllMotors(Conf :: motorconfig, State :: atom) -> ok | error.
setAllMotors(Conf, State) ->	
	if 
		State == stop ->  %%Halt / Stop
			setMotor(a, 0.4),
			setMotor(b, 0.4),
			setMotor(c, 0.4),
			setMotor(d, 0.4),
			setMotor(e, 0.4),
			setMotor(f, 0.4);
		State == flying ->
			setMotor(a, Conf#motorconf.a),
			setMotor(b, Conf#motorconf.b),
			setMotor(c, Conf#motorconf.c),
			setMotor(d, Conf#motorconf.d),
			setMotor(e, Conf#motorconf.e),
			setMotor(f, Conf#motorconf.f);
		true ->
			setMotor(a, 0),
			setMotor(b, 0),
			setMotor(c, 0),
			setMotor(d, 0),
			setMotor(e, 0),
			setMotor(f, 0)
	end,		
ok.

testmotor(Id) ->
	setMotor(Id, conf(test_throttle)),
	timer:sleep(conf(test_timeout)),
	setMotor(Id, conf(stop_throttle)).
