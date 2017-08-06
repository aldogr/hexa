%%%-------------------------------------------------------------
%%% @doc OTP Control gen_server
%%%-------------------------------------------------------------

-module(controller_aldo).  
-behaviour(gen_server).        

 

-define(Ts, 0.001).   
%%%% time it takes in a full loop x 2 of the response rate of the sys (x3 or 5 better)                  

-define(KP_R_PIT, 0.5).   %%%rotation parameter degrees per second
-define(KI_R_PIT, 0.0).
-define(KP_A_PIT, 1.0).   %%%angle (euler) parameter
-define(KI_A_PIT, 0.0).

-define(KP_R_ROL, 0.5). 
-define(KI_R_ROL, 0.0).
-define(KP_A_ROL, 1.0). 
-define(KI_A_ROL, 0.0).


-define(KP_R_YAW, 0.5). 
-define(KI_R_YAW, 0.0).
-define(KP_A_YAW, 1.0). 
-define(KI_A_YAW, 0.0).


%%%% Maximun and min output
-define(YMAX_R, 0.1).
-define(YMIN_R, -0.1).

-define(YMAX_A, 0.1).
-define(YMIN_A, -0.1).


-define(CTRL_TY, 1). %%type of control selected
	%%% 1: PID, else: Cascade PI-PI


%% @TO DO:move all define to sys config
-export([start_link/0]).  
             
-export([                      
  init/1,                      
  handle_call/3,       
  handle_cast/2,       
  handle_info/2,
  terminate/2,      
  code_change/3]).
     
-include("../include/remote.hrl").
-include("../include/imu.hrl").
-include("../include/pwm.hrl").
-include("../include/controller.hrl"). 
-include("../include/sonar.hrl").  
     
timeout() -> 1. %ms

%% -------------------------------------------------------------
%% API Function Definitions
%% -------------------------------------------------------------

start_link() ->                
    gen_server:start_link({local, ?MODULE}, ?MODULE,[], []).  


%% -------------------------------------------------------------
%% gen_server Function Definitions
%% -------------------------------------------------------------

init([]) ->  
	io:format("~p starting~n", [?MODULE]),
	process_flag(trap_exit, true),
	
	
	Pidpitch = #pidstate{kp = ?KP_R_PIT, ki = ?KI_R_PIT, kp_a = ?KP_A_PIT, ki_a = ?KI_A_PIT, t = os:timestamp(), integral = 0, double_integral = 0, last_e = 0},
	Pidyaw = #pidstate{kp = ?KP_R_YAW, ki = ?KI_R_YAW, kp_a = ?KP_A_YAW, ki_a = ?KI_A_YAW, t = os:timestamp(), integral = 0, double_integral = 0, last_e = 0},
	Pidroll = #pidstate{kp = ?KP_R_ROL, ki = ?KI_R_ROL, kp_a = ?KP_A_ROL, ki_a = ?KI_A_ROL, t = os:timestamp(), integral = 0, double_integral = 0, last_e = 0},
	%%% will this be necesary?
	
	Motors = #motorconf{a = 0.0, b = 0.0, c = 0.0, d = 0.0, e = 0.0, f = 0.0},

	St = #controllerstate{
		%timer = Timer,
		pid_pitch = Pidpitch,
		pid_yaw = Pidyaw,
		pid_rollx = Pidroll,
		motors = Motors,
		t = os:timestamp(),  
		fps = 0.0		
	},
	{ok, St}.


         

handle_call(get_data, _From, State) -> 
    {reply, State, State};

handle_call(_What, _From, State) ->
    {reply, false, State}.

%% @doc Starts recurring execution of the controller
handle_cast(start, State) ->
	Timer = erlang:send_after(timeout(), self(), iteration),
	NewS = State#controllerstate{timer = Timer},
	{noreply, NewS};

handle_cast(stop, State) ->
	timer:cancel(State#controllerstate.timer),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(iteration, State) ->
	erlang:cancel_timer(State#controllerstate.timer),
	Now = os:timestamp(),
	Delta = timer:now_diff(Now, State#controllerstate.t), % Microseconds
	LastFps = 1000000.0 / Delta,
	NewFps = 0.25 * LastFps + 0.75 * State#controllerstate.fps,
	
	Nominal_values = gen_server:call(remote, get_nominal_values),   %%% this should be degrees
	Sensor_data = gen_server:call(imu_bno055, update_data),
	%Altitude_data = gen_server:call(hc_sr04, update_height), %%%still unused

	%% Run the controllers
	%% run_controller(parameters, reference, imu:degrees/s, imu:degrees)
%%%@ TO DO nominal values in degrees and degrees per second  BOTH

{SRoll, YRoll} = run_controller(State#controllerstate.pid_rollx,
		Nominal_values#nominal_values.roll_x,
	Sensor_data#imudata.rotation#vector_xyzf.x,Sensor_data#imudata.euler#vector_xyzf.x),

{SPitch, YPitch} = run_controller(SRoll#controllerstate.pid_pitch,
		Nominal_values#nominal_values.pitch,
	Sensor_data#imudata.rotation#vector_xyzf.y,Sensor_data#imudata.euler#vector_xyzf.y),

{SYaw, YYaw} = run_controller(SPitch#controllerstate.pid_yaw,
		Nominal_values#nominal_values.yaw,
	Sensor_data#imudata.rotation#vector_xyzf.z,Sensor_data#imudata.euler#vector_xyzf.z),

  
  	%% Calculate the Motors

	OriginalMotorState = State#controllerstate.motors,
	NewMotorState = calc_motors(OriginalMotorState, YRoll, YPitch, YYaw),

	%% Set Motor
	gen_server:call(pwm, {setAllMotors, NewMotorState}),

	Timer = erlang:send_after(timeout(), self(), iteration), 

	
	NewState = #controllerstate{timer = Timer,
		pid_pitch = SPitch,
		pid_yaw = SYaw,
		pid_rollx = SRoll,
		motors = NewMotorState,
		fps = NewFps,
		t = Now
		
	},
	{noreply, NewState};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->      
	io:format("~p terminating~n", [?MODULE]),
	timer:cancel(State#controllerstate.timer),
	%upid ! {self(), shutdown},   %%%check this
    ok.                        

code_change(_OldVsn, State, _Extra) ->  
   {ok, State}.               

%% -------------------------------------------------------------
%% Internal Function Definitions
%% -------------------------------------------------------------

%% run_controller(parameters, reference, imu:degrees/s, imu:degrees)

run_controller(PidState, NominalValues, SenROT, SenEUL) ->
	%%%here control is picked
%%% TO DO: definitions to sys config

	if
		?CTRL_TY == 1 ->            
			R_Error = NominalValues - SenROT,  %%%%Nom values in degrees/s
			{S, Yout} = ctrl(PidState, R_Error);
		true ->
			A_Error = NominalValues - SenEUL,  %%%%Nom values in degrees
			{S, Yout} = cascade_ctrl(PidState, SenROT, A_Error)
	end,
{S, Yout}.


cascade_ctrl(S, SenR, Ea) ->
	Dt = timer:now_diff(os:timestamp(), S#pidstate.t), %%% both controls share the Dt
	Ia = S#pidstate.integral_a + Ea * ?Ts,
	Ya = S#pidstate.kp_a * Ea + S#pidstate.ki_a * Ia,
	
	if 
		Ya < ?YMIN_A ->
			Ya_1 = ?YMIN_A,
			Ia = S#pidstate.integral_a;
		Ya > ?YMAX_A ->  
			Ya_1 = ?YMAX_A,
			Ia = S#pidstate.integral_a;
		true ->
			Ya_1 = Ya
	end,

	%%%%%%-------------
	%%   second control

	Er = Ya_1 - SenR,
	Ir = S#pidstate.integral + Er * Dt,
	Yr = S#pidstate.kp * Er + S#pidstate.ki * Ir,
	
	if 
		Yr < ?YMIN_R ->
			Y = ?YMIN_R,
			Ir = S#pidstate.integral;
		Yr > ?YMAX_R ->  
			Y = ?YMAX_R,
			Ir = S#pidstate.integral;
		true ->
			Y = Yr
	end,

	NewS = S#pidstate{t = os:timestamp(), integral = Ir, integral_a = Ia, y = Y},
	{ok, NewS, Y}.
	
ctrl(S, Er) ->
	Dt = os:timestamp() - S#pidstate.t,
	Ir = S#pidstate.integral + Er * Dt,
	Dr = (S#pidstate.last_e - Er)/Dt,
	Yr = S#pidstate.kp * Er + S#pidstate.ki * Ir +S#pidstate.kd *Dr,
	
	%%%antiwindup
	if 
		Yr < ?YMIN_R ->
			Y = ?YMIN_R,
			Ir = S#pidstate.integral;
		Yr > ?YMAX_R ->  
			Y = ?YMAX_R,
			Ir = S#pidstate.integral;
		true ->
			Y = Yr
	end,
	
	NewS = S#pidstate{t = os:timestamp(), integral = Ir, y = Y, last_e = Er},
	{ok, NewS, Y}.
	
	
	
%% @doc simply add the value to all motors
%            A         f
%             \       /
%              \     /
%         b-----  ^  -----E
%              /     \
%             /	      \
%            C         d



calc_motors(S, Roll, Pitch, Yaw) -> 
	A = S#motorconf.c - 0.5 * Roll + 0.866 * Pitch + Yaw,
	B = S#motorconf.b - 1 * Roll + 0 * Pitch - Yaw,
	C = S#motorconf.f - 0.5 * Roll - 0.866 * Pitch - Yaw,
	D = S#motorconf.d + 0.5 * Roll - 0.866 * Pitch - Yaw,
	E = S#motorconf.a + 1 * Roll + 0 * Pitch + Yaw,
	F = S#motorconf.e + 0.5 * Roll + 0.866 * Pitch + Yaw,

	_NewMotor = S#motorconf{a = A, b = B, c = C, d = D, e = E, f = F}.


      


      


 
