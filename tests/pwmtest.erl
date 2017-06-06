-module(pwmtest).
-export([setMotor/2, enable_all/0, setAllMotors/1]).
-define(COUNT_OF_MOTORS, 6).

frequency() -> 400.
%chips() -> [0, 2, 4].
%outputs() -> [0, 1].
motors() -> [{a, 0, 0}, {b, 0, 1}, {c, 2, 0}, {d, 2, 1}, {e, 4, 0}, {f, 4, 1}].
prefix() -> "/sys/class/pwm/pwmchip".

enable(Chip, Output) ->
	file:write_file(prefix()++integer_to_list(Chip)++"/pwm"++integer_to_list(Output)++"/enable", "1").

enable_all() ->
	io:format("Set motor duty to 0~n"),
	[setMotor(Id, 0) || {Id, _, _} <- motors()],
	io:format("Enable all motors~n"),
	[enable(Chip, Output) || {_Motor, Chip, Output} <- motors()],
	ok.

setMotor(Id, Percentage) when
		Percentage >= 0,
		Percentage =< 1 ->
	case [true || {CurrentId, _, _} <- motors(), CurrentId =:= Id] of
		[true] ->
			Duty = float_to_list(Percentage * 1.0e9 / frequency(), [{decimals, 0}]),
			[{Chip, Output}] = [{Chip, Output} || {CurrentId, Chip, Output} <- motors(), Id =:= CurrentId],
			file:write_file(prefix()++integer_to_list(Chip)++"/pwm"++integer_to_list(Output)++"/duty_cycle", Duty),
			ok;
		[] -> wrongMotor
	end;

setMotor(_Id, _Percentage) -> wrongParameters.

setMotor(Id, Percentage, Addr) ->
	%io:format("setMotor/3 ~p~n", [Id]),
	Addr ! {Id, setMotor(Id, Percentage)}.

%% @doc Updates all Motors simultaneously by spawning processes
%% @spec setAllMotors(Parameters :: List) -> atom()
%% where
%% 	List = {Id :: atom(), Value :: float()}
%%	Result = Atom
setAllMotors(Parameters) ->
	Me = self(),
	case length(Parameters) of
		?COUNT_OF_MOTORS ->
			Collector = spawn(fun () -> collector(Me,0) end),
			[spawn(fun() -> setMotor(Motor, Val, Collector) end) || {Motor, Val} <- Parameters],
			receive
				ok -> ok;
				{Id, Error} -> {Id, Error};
				error -> error
			end;
		_ -> throw(wrongCountOfMotors)
	end.

collector(Addr, Nr) when Nr < ?COUNT_OF_MOTORS, Nr >= 0 ->
	%io:format("Started collector nr ~B~n", [Nr]),
	receive
		{Id, wrongMotor} -> Addr ! {Id, wrongMotor};
		{Id, wrongParameters} -> Addr ! {Id, wrongParameters};
		{_Id, ok} -> collector(Addr, Nr+1)
		%Else -> io:format("Collector received: ~s~n", [Else])
	end;
	
collector(Addr, Nr) ->
	%io:format("Collector last run~n"),
	case Nr of
		?COUNT_OF_MOTORS -> Addr ! ok;
		_ -> Addr ! error
	end.
