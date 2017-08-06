-module(controller_kilian).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../include/remote.hrl").
-include("../include/imu.hrl").
-include("../include/pwm.hrl").
-include("../include/controller.hrl").


timeout() -> 1. %Milliseconds

% Config
conf(yaw_kp) ->
	{ok, Val} = application:get_env(k_yaw_kp),
	Val;
	
conf(yaw_ki) ->
	{ok, Val} = application:get_env(k_yaw_ki),
	Val;

conf(yaw_kd) ->
	{ok, Val} = application:get_env(k_yaw_kd),
	Val;

conf(rollx_kp) ->
	{ok, Val} = application:get_env(k_rollx_kp),
	Val;

conf(rollx_ki) ->
	{ok, Val} = application:get_env(k_rollx_ki),
	Val;

conf(rollx_kd) ->
	{ok, Val} = application:get_env(k_rollx_kd),
	Val;

conf(rolly_kp) ->
	{ok, Val} = application:get_env(k_rolly_kp),
	Val;

conf(rolly_ki) ->
	{ok, Val} = application:get_env(k_rolly_ki),
	Val;

conf(rolly_kd) ->
	{ok, Val} = application:get_env(k_rolly_kd),
	Val;

conf(mmin) ->
	{ok, Val} = application:get_env(k_mmin),
	Val;

conf(mmax) ->
	{ok, Val} = application:get_env(k_mmax),
	Val;

conf(imin) ->
	{ok, Val} = application:get_env(k_imin),
	Val;

conf(imax) ->
	{ok, Val} = application:get_env(k_imax),
	Val;

conf(ymin) ->
	{ok, Val} = application:get_env(k_ymin),
	Val;

conf(ymax) ->
	{ok, Val} = application:get_env(k_ymax),
	Val.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("~p starting~n", [?MODULE]),
	process_flag(trap_exit, true),
	
	Pidyaw = #pidstate{
		kp = conf(yaw_kp),
		ki = conf(yaw_ki),
		kd = conf(yaw_kd),
		t = now(), integral = 0, double_integral = 0,
		last_e = 0, y = 0
	},
	Pidrollx = #pidstate{
		kp = conf(rollx_kp),
		ki = conf(rollx_ki),
		kd = conf(rollx_kd),
		t = now(), integral = 0, double_integral = 0,
		last_e = 0, y = 0
	},
	Pidrolly = #pidstate{
		kp = conf(rolly_kp),
		ki = conf(rolly_ki),
		kd = conf(rolly_kd),
		t = now(), integral = 0, double_integral = 0,
		last_e = 0, y = 0
	},
	Motors = #motorconf{a = 0.0, b = 0.0, c = 0.0, d = 0.0, e = 0.0, f = 0.0},
	
	St = #controllerstate{
		timer = undefined,
		pid_yaw = Pidyaw,
		pid_rollx = Pidrollx,
		pid_rolly = Pidrolly,
		motors = Motors,
		t = now(),
		fps = 0.0
	},
	{ok, St}.

handle_call(get_data, _From, State) ->
	{reply, State, State};

handle_call(_What, _From, State) ->
    {reply, false, State}.

%% @doc Starts recurring execution of the controller
handle_cast(start, State) ->
	%% @TODO Remove forcing the state
	gen_server:cast(pwm, {setState, flying}),
	Timer = erlang:send_after(timeout(), self(), iteration),
	NewS = State#controllerstate{timer = Timer},
	{noreply, NewS};

%% @doc Stops recurring execution
handle_cast(stop, State) ->
	%% @TODO same as above	
	gen_server:cast(pwm, {setState, stop}),
	case State#controllerstate.timer of
		undefined ->
			{noreply, State};
		Timer ->
			erlang:cancel_timer(Timer),
			{noreply, State#controllerstate{timer = undefined}}
	end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(iteration, State) ->
	erlang:cancel_timer(State#controllerstate.timer),
	Now = now(),
	Delta = timer:now_diff(Now, State#controllerstate.t), % Microseconds
	LastFps = 1000000.0 / Delta,
	%% Make it a bit less jittery
	NewFps = 0.25 * LastFps + 0.75 * State#controllerstate.fps,
	
	Nominal_values = gen_server:call(remote, get_nominal_values),
	Sensor_data = gen_server:call(imu_bno055, update_data),
	
	OriginalMotorState = #motorconf{a = 0.0, b = 0.0, c = 0.0, d = 0.0, e = 0.0, f = 0.0},
	
	PitchMotorState = calc_motors(pitch, OriginalMotorState,
		0.2 * (Nominal_values#nominal_values.pitch + 3)), %-1.0...1.0->0.4...0.8
	
	%% Run the controllers
	{ok, SRollx, YRollx} = run_controller(State#controllerstate.pid_rollx,
		Nominal_values#nominal_values.roll_x,
		-Sensor_data#imudata.rotation#vector_xyzf.y,
		15.0),%dps
	{ok, SRolly, YRolly} = run_controller(State#controllerstate.pid_rolly,
		Nominal_values#nominal_values.roll_y,
		-Sensor_data#imudata.rotation#vector_xyzf.x,
		15.0),%dps
	
	RollMotorState = calc_motors(roll, PitchMotorState, YRollx, YRolly),
	
	{ok, SYaw, YYaw} = run_controller(State#controllerstate.pid_yaw,
		Nominal_values#nominal_values.yaw,
		-Sensor_data#imudata.rotation#vector_xyzf.z,
		8.0),%dps
	YawMotorState = calc_motors(yaw, RollMotorState, YYaw),
	
	%Disable Pitch controller for first tests
	%{ok, SPitch, YPitch} = run_controller(State#controllerstate.pid_pitch,
	%	Nominal_values#nominal_values.pitch,
	%	Sensor_data#imudata.linear_acceleration#vector_xyzf.z,
	%	1.0),%acceleration
	%	
	%PitchMotorState = calc_motors(pitch, YawMotorState, YPitch),
	
	FinalMotorState = YawMotorState,
	%% Set Motors:
	gen_server:cast(pwm, {setAllMotors, FinalMotorState}),
	%% @TODO Fine-Tune timeout
	Timer = erlang:send_after(timeout(), self(), iteration), %% Pause the execution, time for sth else
	
	NewState = #controllerstate{timer = Timer,
		%pid_pitch = SPitch,
		pid_yaw = SYaw,
		pid_rollx = SRollx,
		pid_rolly = SRolly,
		motors = FinalMotorState,
		t = Now,
		fps = NewFps
	},
	{noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
	io:format("~p stopping~n", [?MODULE]),
	timer:cancel(State#controllerstate.timer),
	%upid ! {self(), shutdown},
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

run_controller(PidState, NominalValues, Sensor, Scaling) ->
	E_pitch = Scaling * NominalValues - Sensor,
	{ok ,_NewS, _Y} = pid(PidState,  E_pitch).

pid(S, E) ->
	Dt = timer:now_diff(now(), S#pidstate.t),
	Integral = limit(S#pidstate.integral + E * Dt,
		conf(imin),
		conf(imax)
	),
	Derivative = (S#pidstate.last_e - E)/Dt,
	Y = limit(S#pidstate.kd * Derivative + S#pidstate.kp * E + S#pidstate.ki * Integral,
		conf(ymin),
		conf(ymax)
	),
	NewS = S#pidstate{t = now(), integral = Integral, y = Y, last_e = E},
	{ok, NewS, Y}.

%pid_integral(S, E) ->
	%Dt = timer:now_diff(now(), S#pidstate.t),
	%Integral = S#pidstate.integral + E * Dt,
	%Double_Integral = S#pidstate.double_integral + Integral * Dt,
	%Y = S#pidstate.kd * E + S#pidstate.kp * Integral + S#pidstate.ki * Double_Integral,
	%	 acc					vel							%position
	%   rot					ang							%
	%NewS = S#pidstate{t = now(), integral = Integral,
		%double_integral = Double_Integral, y = Y},
	%{ok, NewS, Y}.

%% @returns new #motorconf
%% @doc simply add the value to all motors
calc_motors(pitch, S, Y) ->
	A = S#motorconf.a + Y,
	B = S#motorconf.b + Y,
	C = S#motorconf.c + Y,
	D = S#motorconf.d + Y,
	E = S#motorconf.e + Y,
	F = S#motorconf.f + Y,
	NewMotor = S#motorconf{a = A, b = B, c = C, d = D, e = E, f = F},
	_LimitedMotor = limit_motors(NewMotor);

%% @doc add to right rotating, substract from left rotating.
calc_motors(yaw, S, Y) ->
	Z = Y/2,
	A = S#motorconf.a + Z,
	B = S#motorconf.b - Z,
	C = S#motorconf.c + Z,
	D = S#motorconf.d - Z,
	E = S#motorconf.e + Z,
	F = S#motorconf.f - Z,
	NewMotor = S#motorconf{a = A, b = B, c = C, d = D, e = E, f = F},
	_LimitedMotor = limit_motors(NewMotor).

%% @doc Combine X and Y. X roll. Y is done by motors b(forw) and e(backw).
%% 		Y is done by rotors a(backw), c(forw) and d(forw), and f(backw).
calc_motors(roll, S, Yx, Yy) -> 
	A = S#motorconf.a - Yx,
	B = S#motorconf.b - Yy,
	C = S#motorconf.c + Yx,
	D = S#motorconf.d + Yx,
	E = S#motorconf.e + Yy,
	F = S#motorconf.f - Yx,
	NewMotor = S#motorconf{a = A, b = B, c = C, d = D, e = E, f = F},
	_LimitedMotor = limit_motors(NewMotor).

limit_motors(Motors) ->
	[motorconf|MList] = tuple_to_list(Motors),
	limit_motors(MList, []).

limit_motors([], Conf) ->
	list_to_tuple([motorconf|Conf]);
	
limit_motors([H|T], Conf) ->
	Limited = limit(H, conf(mmin), conf(mmax)),
	limit_motors(T, Conf ++ [Limited]).

limit(Val, Min, Max) ->
	if
		Val >= Max -> Max;
		Val =< Min -> Min;
		true -> Val
	end.
