%%%-------------------------------------------------------------
%%% @doc OTP Control gen_server
%%%-------------------------------------------------------------

-module(controller_aldo).  
-behaviour(gen_server).        
                               
 

-define(Ts, 0.001).   
%%%% time it takes in a full loop x 2 of the response rate of the sys (x3 or 5 better)                  

-define(KP_R_PIT, 0.5).   %%%rotation parameter
-define(KI_R_PIT, 1).
-define(KP_A_PIT, 0.5).   %%%angle (euler) parameter
-define(KI_A_PIT, 1).

-define(KP_R_ROL, 0.5). 
-define(KI_R_ROL, 1).
-define(KP_A_ROL, 0.5). 
-define(KI_A_ROL, 1).


-define(KP_R_YAW, 0.5). 
-define(KI_R_YAW, 1).
-define(KP_A_YAW, 0.5). 
-define(KI_A_YAW, 1).

-define(IMAX, 10).
-define(IMIN, -10).

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
     
timeout() -> 20. %2ms

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
	
	
	Timer = erlang:send_after(10, self(), iteration),
	St = #controllerstate.timer = Timer,
	
	Pidpitch = #pidstate{kp = ?KP_R_PIT, ki = ?KI_R_PIT, kp_a = ?KP_A_PIT, ki_a = ?KI_A_PIT},

	Pidyaw = #pidstate{kp = ?KP_R_YAW, ki = ?KI_R_YAW, kp_a = ?KP_A_YAW, ki_a = ?KI_A_YAW},

	Pidroll = #pidstate{kp = ?KP_R_ROL, ki = ?KI_R_ROL, kp_a = ?KP_A_ROL, ki_a = ?KI_A_ROL},

	
	St = #controllerstate{
		timer = Timer,
		pid_pitch = Pidpitch,
		pid_yaw = Pidyaw,
		pid_rollx = Pidroll
		
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


{SRoll, YRoll} = run_controller(State#controllerstate.pid_rollx,
		Nominal_values#nominal_values.roll_x,
	Sensor_data#imudata.rotation#vector_xyz.x,Sensor_data#imudata.euler#vector_xyz.x),

{SPitch, YPitch} = run_controller(SRoll#controllerstate.pid_pitch,
		Nominal_values#nominal_values.pitch,
	Sensor_data#imudata.rotation#vector_xyz.y,Sensor_data#imudata.euler#vector_xyz.y),

{SYaw, YYaw} = run_controller(SPitch#controllerstate.pid_yaw,
		Nominal_values#nominal_values.yaw,
	Sensor_data#imudata.rotation#vector_xyz.z,Sensor_data#imudata.euler#vector_xyz.z),

  %%%%%%
  %%%%%%


	OriginalMotorState = State#controllerstate.motors,
	NewMotorState = calc_motors(OriginalMotorState, YRoll, YPitch, YYaw),

  


%% Set Motors:
	gen_server:call(pwm, {setAllMotors, NewMotorState}),
%% @TODO Fine-Tune timeout

	Timer = erlang:send_after(timeout()), 
%% Pause the execution, time for sth else
	
	NewState = #controllerstate{timer = Timer,
		pid_pitch = SPitch,
		pid_yaw = SYaw,
		pid_rollx = SRoll,
		motors = NewMotorState
	},
	{noreply, NewState};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->      
	io:format("~p terminating~n", [?MODULE]),
	%timer:cancel(State),
	upid ! {self(), shutdown},
    ok.                        

code_change(_OldVsn, State, _Extra) ->     {ok, State}.               

%% -------------------------------------------------------------
%% Internal Function Definitions
%% -------------------------------------------------------------

run_controller(PidState, NominalValues, SenROT, SenEUL) ->
	Error = NominalValues - SenEUL,
	{_NewS, _Y} = ctrl(PidState,  SenROT, Error).


ctrl(S, SenR, Ea) ->

	%%%Dt = now() - S#pidstate.t,

	Ia = S#pidstate.integral_a + Ea * ?Ts,
	I2a = max(min(S#pidstate.imax,Ia),S#pidstate.imin),  
%%%antiwindup

	Ya = S#pidstate.kp_a * Ea + S#pidstate.ki_a * I2a,

%%%%%%%%%%%%%%%%%%%
%%%%%second control
%%%%%%%%%%%%%%%%%%%

	Er = Ya - SenR,
	Ir = S#pidstate.integral + Er * ?Ts,
	I2r = max(min(S#pidstate.imax,Ir),S#pidstate.imin),  
%%%antiwindup

	Y = S#pidstate.kp * Er + S#pidstate.ki * I2r,


	NewS = S#pidstate{t = now(), integral = I2r, integral_a = I2a, y = Y},

	{ok, NewS, Y}.
	
	





%% @doc simply add the value to all motors, check parameters!

%% Y is done by rotors b(backw),c(forw) and e(forw),and f(backw)

calc_motors(S, roll, pitch, yaw) -> 
	A = S#motorconf.a + 1 * roll + 0 * pitch + yaw,
	B = S#motorconf.b - 1 * roll + 0 * pitch - yaw,
	C = S#motorconf.c - 0.5 * roll + 0.866 * pitch + yaw,
	D = S#motorconf.d + 0.5 * roll - 0.866 * pitch - yaw,
	E = S#motorconf.e + 0.5 * roll + 0.866 * pitch + yaw,
	F = S#motorconf.f - 0.5 * roll - 0.866 * pitch - yaw,
	_NewMotor = S#motorconf{a = A, b = B, c = C, d = D, e = E, f = F}.


      


      


 