-module(pwm).
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../include/pwm.hrl").

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
		


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @TODO: Also execute enable_all, check if setup scripts were executed.
init([]) ->
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
    {ok, 0}.

handle_call({enable_all}, _From, State) ->
    {reply, enable_all(), State};
    
handle_call({setAllMotors, Conf}, _From, State) ->
	{reply, setAllMotors(Conf), State}.

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
	%case [true || {CurrentId, _, _} <- motors(), CurrentId =:= Id] of
		%[true] ->
			%Duty = float_to_list(Percentage * 1.0e9 / frequency(),
				%[{decimals, 0}]),
			%[{Chip, Output}] = [{Chip, Output} || {CurrentId, Chip, Output}
				%<- motors(), Id =:= CurrentId],
			%file:write_file(prefix()++integer_to_list(Chip)++"/pwm"++
				%integer_to_list(Output)++"/duty_cycle", Duty),
			%{Id, ok};
		%[] -> wrongMotor
	%end.

%% @doc Updates all motors
%% @param Conf Record motorconifg, Values between 0..1
-spec setAllMotors(Conf :: motorconfig) -> ok | error.
setAllMotors(Conf) ->
	setMotor(a, Conf#motorconf.a),
	setMotor(b, Conf#motorconf.b),
	setMotor(c, Conf#motorconf.c),
	setMotor(d, Conf#motorconf.d),
	setMotor(e, Conf#motorconf.e),
	setMotor(f, Conf#motorconf.f),
	ok.
