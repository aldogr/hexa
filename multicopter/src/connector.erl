-module(connector).
-behavior(gen_server).

-include("../include/imu.hrl").
-include("../include/pwm.hrl").
-include("../include/controller.hrl").
%% @TODO: Transfer all gen_tcp stuff to use actual OTP behaviour!


-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("~p starting~n", [?MODULE]),
	process_flag(trap_exit, true),
	{ok, Listen} = gen_tcp:listen(12000, [binary,
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

%% @TODO Remove debug output
loop(Socket) ->
	receive
		{tcp, Socket, Bin = <<"GET">>} ->
			io:format("Server received binary = ~p~n", [Bin]),
			{ok, ImoData} = gen_server:call(imu_bno055, get_data),
			{ok, ControllerData} = gen_server:call(controller, get_data),
			io:format("Got data, sending reply~n"),
			{ok, BinaryData} = format_data(ImoData, ControllerData),
			gen_tcp:send(Socket, BinaryData),
			io:format("Sent to the socket: ~p~n", [BinaryData]),
			loop(Socket);
		{tcp_closed, Socket} ->
			io:format("Socket closed~n")
	end.


-spec format_data(_ImoData :: #imudata{}, ControllerData :: #controllerstate{} ) -> {ok, _BinaryData :: <<>>} | {error, _Error}.
format_data(ImoData, ControllerData) ->
	{ok,
	<<
	(ImoData#imudata.gravity#vector_xyz.x):2/little-signed-integer-unit:8,
	(ImoData#imudata.gravity#vector_xyz.y):2/little-signed-integer-unit:8,
	(ImoData#imudata.gravity#vector_xyz.z):2/little-signed-integer-unit:8,
	(ImoData#imudata.acceleration#vector_xyz.x):2/little-signed-integer-unit:8,
	(ImoData#imudata.acceleration#vector_xyz.y):2/little-signed-integer-unit:8,
	(ImoData#imudata.acceleration#vector_xyz.z):2/little-signed-integer-unit:8,
	(ImoData#imudata.magnet#vector_xyz.x):2/little-signed-integer-unit:8,
	(ImoData#imudata.magnet#vector_xyz.y):2/little-signed-integer-unit:8,
	(ImoData#imudata.magnet#vector_xyz.z):2/little-signed-integer-unit:8,
	(ImoData#imudata.rotation#vector_xyz.x):2/little-signed-integer-unit:8,
	(ImoData#imudata.rotation#vector_xyz.y):2/little-signed-integer-unit:8,
	(ImoData#imudata.rotation#vector_xyz.z):2/little-signed-integer-unit:8,
	(ImoData#imudata.linear_acceleration#vector_xyz.x):2/little-signed-integer-unit:8,
	(ImoData#imudata.linear_acceleration#vector_xyz.y):2/little-signed-integer-unit:8,
	(ImoData#imudata.linear_acceleration#vector_xyz.z):2/little-signed-integer-unit:8,
	(ImoData#imudata.temperature):2/little-signed-integer-unit:8,
	(ImoData#imudata.euler#euler.heading):2/little-signed-integer-unit:8,
	(ImoData#imudata.euler#euler.roll):2/little-signed-integer-unit:8,
	(ImoData#imudata.euler#euler.pitch):2/little-signed-integer-unit:8,
	(ControllerData#controllerstate.motors#motorconf.a)/float,
	(ControllerData#controllerstate.motors#motorconf.b)/float,
	(ControllerData#controllerstate.motors#motorconf.c)/float,
	(ControllerData#controllerstate.motors#motorconf.d)/float,
	(ControllerData#controllerstate.motors#motorconf.e)/float,
	(ControllerData#controllerstate.motors#motorconf.f)/float,
	(ControllerData#controllerstate.pid_pitch#pidstate.y)/float,
	(ControllerData#controllerstate.pid_yaw#pidstate.y)/float,
	(ControllerData#controllerstate.pid_rollx#pidstate.y)/float,
	(ControllerData#controllerstate.pid_rolly#pidstate.y)/float
	>>}.
