-module(connector).
-behavior(gen_server).

-include("../include/common.hrl").
-include("../include/imu.hrl").
-include("../include/pwm.hrl").
-include("../include/controller.hrl").
-include("../include/remote.hrl").
-include("../include/sonar.hrl").


%% @TODO: Transfer all gen_tcp stuff to use actual OTP behaviour!
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% Config values:
conf(port) ->
	{ok, Port} = application:get_env(connector_port),
	Port;

conf(timeout) ->
	{ok, Timeout} = application:get_env(connector_timeout),
	Timeout.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("~p starting~n", [?MODULE]),
	process_flag(trap_exit, true),
	{ok, Listen} = gen_tcp:listen(conf(port), [binary,
		{reuseaddr, true},
		{active, true}]),
	spawn(fun() -> connect(Listen) end),
	{ok, Listen}.

handle_call(_What, _From, State) ->
    {reply, false, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
	io:format("~p stopping~n", [?MODULE]),
	inet:close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connect(Listen) -> 	
	{ok, Socket} = gen_tcp:accept(Listen),
	spawn(fun() -> connect(Listen) end),
	loop(Socket).

%% @TODO Make getter calls async (use cast)
loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			process(Socket, Bin),
			loop(Socket);
			
		{tcp_closed, Socket} ->
			io:format("Socket closed~n"),
			emergency()
	after conf(timeout) -> 
		emergency(),
		inet:close(Socket)
	end.

emergency() ->
		gen_server:cast(?CONTROLLERMODULE, stop),
		gen_server:cast(pwm, {setState, stop}),
		io:format("EMERGENCY STOP~n").


process(Socket, <<"GET", Tail/binary>>) ->
	ImuData = gen_server:call(imu_bno055, get_data),
	HeightData = gen_server:call(hc_sr04, get_height), %%untested
	ControllerData = gen_server:call(?CONTROLLERMODULE, get_data),
	RemoteData = gen_server:call(remote, get_nominal_values),
	MotorData = pwm:getMotors(),
	{ok, BinaryData} = format_data(ImuData, HeightData, ControllerData, RemoteData, MotorData),
	gen_tcp:send(Socket, BinaryData),
	process(Socket, Tail);
	
process(Socket, _Bin = <<"JOYSTICK", Pitch/float, Yaw/float,
				Roll_x/float, Roll_y/float, IEmergency/integer, Tail/binary>>) ->
	Val = #nominal_values{pitch = Pitch, yaw = Yaw,
	roll_x = Roll_x, roll_y = Roll_y,
	emergency = case IEmergency of
		0 -> false;
		_ -> true
	end},
	gen_server:cast(remote, {set, Val}),
	process(Socket, Tail);

% We are done with processing
process(_Socket, <<>>) ->
	ok.

-spec format_data(_ImuData :: #imudata{}, HeightData :: #sonar{},
	ControllerData :: #controllerstate{}, RemoteData :: #nominal_values{},
	MotordData :: #motorconf{})
		-> {ok, _BinaryData :: <<>>} | {error, _Error}.
format_data(ImuData, HeightData, ControllerData, RemoteData, MotorData) ->
	IEmergency = case RemoteData#nominal_values.emergency of
		true -> 1;
		false -> 0
	end,
	{ok,
	<<
	(ImuData#imudata.gravity#vector_xyzf.x)/float,
	(ImuData#imudata.gravity#vector_xyzf.y)/float,
	(ImuData#imudata.gravity#vector_xyzf.z)/float,
	
	(ImuData#imudata.acceleration#vector_xyzf.x)/float,
	(ImuData#imudata.acceleration#vector_xyzf.y)/float,
	(ImuData#imudata.acceleration#vector_xyzf.z)/float,
	
	(ImuData#imudata.magnet#vector_xyz.x):2/big-signed-integer-unit:8,
	(ImuData#imudata.magnet#vector_xyz.y):2/big-signed-integer-unit:8,
	(ImuData#imudata.magnet#vector_xyz.z):2/big-signed-integer-unit:8,
	
	(ImuData#imudata.rotation#vector_xyzf.x)/float,
	(ImuData#imudata.rotation#vector_xyzf.y)/float,
	(ImuData#imudata.rotation#vector_xyzf.z)/float,
	
	(ImuData#imudata.linear_acceleration#vector_xyzf.x)/float,
	(ImuData#imudata.linear_acceleration#vector_xyzf.y)/float,
	(ImuData#imudata.linear_acceleration#vector_xyzf.z)/float,
	
	(ImuData#imudata.temperature):2/big-signed-integer-unit:8,
	
	(ImuData#imudata.euler#euler.heading)/float,
	(ImuData#imudata.euler#euler.roll)/float,
	(ImuData#imudata.euler#euler.pitch)/float,

	(HeightData#sonar.height)/float,
	
	(MotorData#motorconf.a)/float,
	(MotorData#motorconf.b)/float,
	(MotorData#motorconf.c)/float,
	(MotorData#motorconf.d)/float,
	(MotorData#motorconf.e)/float,
	(MotorData#motorconf.f)/float,
	
	%(ControllerData#controllerstate.pid_pitch#pidstate.y)/float,
	(ControllerData#controllerstate.pid_yaw#pidstate.y)/float,
	(ControllerData#controllerstate.pid_rollx#pidstate.y)/float,
	(ControllerData#controllerstate.pid_rolly#pidstate.y)/float,
	
	(ControllerData#controllerstate.fps)/float,
	
	%(ControllerData#controllerstate.pid_pitch#pidstate.integral)/float,
	(ControllerData#controllerstate.pid_yaw#pidstate.integral)/float,
	(ControllerData#controllerstate.pid_rollx#pidstate.integral)/float,
	(ControllerData#controllerstate.pid_rolly#pidstate.integral)/float,
	
	(RemoteData#nominal_values.pitch)/float,
	(RemoteData#nominal_values.yaw)/float,
	(RemoteData#nominal_values.roll_x)/float,
	(RemoteData#nominal_values.roll_y)/float,
	IEmergency/integer
	>>}.
