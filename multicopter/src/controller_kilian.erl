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

timeout() -> 20. %2ms


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Controller Parameters
-define(PITCH_KP, 0).
-define(PITCH_KI, 0).
-define(PITCH_KD, 0).

-define(YAW_KP, 0).
-define(YAW_KI, 0).
-define(YAW_KD, 0).

-define(ROLLX_KP, 0).
-define(ROLLX_KI, 0).
-define(ROLLX_KD, 0).

-define(ROLLY_KP, 0).
-define(ROLLY_KI, 0).
-define(ROLLY_KD, 0).

%%%%%%%%%%%%%%%%%%%%



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @TODO: Trap exit, of spawned, so the whole thing crashes and gets restarted!
%% Stabilize Rot_z with Yaw
%% Stabilize Rot_x,y with Roll
%% Stabilize Lin_Acc z with Pitch
%% PID eg: y(t) = K_d * Rot_z + K_p * Int( Rot_t * dt) + K_i * Int ( Int( Rot_z * dt) * dt )
%% PID with Sums:
%% E_Rot_z = Soll_Rot_z - Sens_Rot_z
%% Dt = T_now - T_alt
%% Integral = Integral + E_Rot_z * Dt
%% Double_Integral = Double_Integral + Dt * Integral
%% T = T_now - T_start
%% y(t) = K_d * E_Rot_z + K_p * Integral + K_i * Double_Integral

%% Stabilize Rot x,y with Gravity vector
%% stabilize yaw with magnet? -> project xyz components on area of gravity vector for heading (length doesn't play a role, microtesla)
%% different controller for height? PD????


init([]) ->
	io:format("~p starting~n", [?MODULE]),
	process_flag(trap_exit, true),
	
	%register(upid, spawn(fun() -> updater() end)),
	%MyId = self(),
	%Tref = timer:send_interval(100, upid, {MyId, update}),
	Timer = erlang:send_after(10, self(), iteration),
	St = #controllerstate.timer = Timer,
	
	Pidpitch = #pidstate{kp = ?PITCH_KP, ki = ?PITCH_KI, kd = ?PITCH_KD},
	Pidyaw = #pidstate{kp = ?YAW_KP, ki = ?YAW_KI, kd = ?YAW_KD},
	Pidrollx = #pidstate{kp = ?ROLLX_KP, ki = ?ROLLX_KI, kd = ?ROLLX_KD},
	Pidrolly = #pidstate{kp = ?ROLLY_KP, ki = ?ROLLY_KI, kd = ?ROLLY_KD},
	
	St = #controllerstate{
		timer = Timer,
		pid_pitch = Pidpitch,
		pid_yaw = Pidyaw,
		pid_rollx = Pidrollx,
		pid_rolly = Pidrolly
	},
	{ok, St}.

handle_call(get_data, _From, State) ->
	{reply, State, State};

handle_call(_What, _From, State) ->
    {reply, false, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(iteration, State) ->
	erlang:cancel_timer(State#controllerstate.timer),
	
	{ok, Nominal_values} = gen_server:call(remote, get_nominal_values),
	{ok, Sensor_data} = gen_server:call(imo_bno055, update_data),
	
	%% Run the controllers
	{SPitch, YPitch} = run_controller(State#controllerstate.pid_pitch,
		%% integrate this???
		Nominal_values#nominal_values.pitch,
		Sensor_data#imudata.linear_acceleration#vector_xyz.z),
	{SYaw, YYaw} = run_controller(SPitch#controllerstate.pid_yaw,
		Nominal_values#nominal_values.yaw,
		Sensor_data#imudata.rotation#vector_xyz.z),
	{SRollx, YRollx} = run_controller(SYaw#controllerstate.pid_rollx,
		Nominal_values#nominal_values.roll_x,
		Sensor_data#imudata.rotation#vector_xyz.x),
	{SRolly, YRolly} = run_controller(SRollx#controllerstate.pid_rolly,
		Nominal_values#nominal_values.roll_y,
		Sensor_data#imudata.rotation#vector_xyz.y),

	%% @TODO Implement Y_i_max!

	%% Calculate new motor duties
	%% @TODO Does the order even matter?

	OriginalMotorState = State#controllerstate.motors,
	RollMotorState = calc_motors(roll, OriginalMotorState, YRollx, YRolly),
	YawMotorState = calc_motors(yaw, RollMotorState#controllerstate.motors, YYaw),
	PitchMotorState = calc_motors(pitch, YawMotorState#controllerstate.motors, YPitch),
	
	%% Set Motors:
	gen_server:call(pwm, {setAllMotors, PitchMotorState}),
	%% @TODO Fine-Tune timeout
	Timer = erlang:send_after(timeout()), %% Pause the execution, time for sth else
	
	NewState = #controllerstate{timer = Timer,
		pid_pitch = SPitch,
		pid_yaw = SYaw,
		pid_rollx = SRollx,
		pid_rolly = SRolly,
		motors = PitchMotorState
	},
	{noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
	io:format("~p stopping~n", [?MODULE]),
	timer:cancel(State),
	upid ! {self(), shutdown},
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%~ updater() ->
	%~ receive
		%~ {_From, update} ->
			%~ gen_server:call(imu_bno055, update_data),
			%~ updater();
		%~ {_From, shutdown} -> ok
	%~ end.
%calc error
%call pid cntrstat and K_is
%ret output
%set output

run_controller(PidState, NominalValues, Sensor) ->
	E_pitch = NominalValues - Sensor,
	{_NewS, _Y} = pid_integral(PidState,  E_pitch).

pid(S, E) ->
	Dt = now() - S#pidstate.t,
	Integral = S#pidstate.integral + E * Dt,
	Derivative = (S#pidstate.last_e - E)/E,
	Y = S#pidstate.kd * Derivative + S#pidstate.kp * E + S#pidstate.ki * Integral,
	NewS = S#pidstate{t = now(), integral = Integral, y = Y, last_e = E},
	{ok, NewS, Y}.
	
pid_integral(S, E) ->
	Dt = now() - S#pidstate.t,
	Integral = S#pidstate.integral + E * Dt,
	Double_Integral = S#pidstate.double_integral + Integral * Dt,
	Y = S#pidstate.kd * E + S#pidstate.kp * Integral + S#pidstate.ki * Double_Integral,
	%%	 acc					vel							%position
	%%   rot					ang							%
	NewS = S#pidstate{t = now(), integral = Integral,
		double_integral = Double_Integral, y = Y},
	{ok, NewS, Y}.
	

%% @returns new #motorconf
%% @doc simply add the value to all motors
calc_motors(pitch, S, Y) ->
	A = S#motorconf.a + Y,
	B = S#motorconf.b + Y,
	C = S#motorconf.c + Y,
	D = S#motorconf.d + Y,
	E = S#motorconf.e + Y,
	F = S#motorconf.f + Y,
	_NewMotor = S#motorconf{a = A, b = B, c = C, d = D, e = E, f = F};

%% @doc add to right rotating, substract from left rotating.
calc_motors(yaw, S, Y) ->
	Z = Y/2,
	A = S#motorconf.a + Z,
	B = S#motorconf.b - Z,
	C = S#motorconf.c + Z,
	D = S#motorconf.d - Z,
	E = S#motorconf.e + Z,
	F = S#motorconf.f - Z,
	_NewMotor = S#motorconf{a = A, b = B, c = C, d = D, e = E, f = F}.

%% @doc Combine X and Y. X roll. X is done by motors a(forw) and d(backw).
%% 		Y is done by rotors b(backw), c(forw) and e(forw), and f(backw).
calc_motors(roll, S, Yx, Yy) -> 
	A = S#motorconf.a - Yx,
	B = S#motorconf.b - Yy,
	C = S#motorconf.c - Yy,
	D = S#motorconf.d + Yx,
	E = S#motorconf.e + Yy,
	F = S#motorconf.f + Yy,
	_NewMotor = S#motorconf{a = A, b = B, c = C, d = D, e = E, f = F}.
