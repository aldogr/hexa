-module(imu_bno055).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("i2c/include/i2c.hrl").
-include("../include/imu.hrl").

-define(I2CBUS, 2).
-define(I2CADDRESS, 16#28).


%%%%%%
% Registers
%%%%%
-define(REG_OPR_MODE, 16#3D).

% Acceleration
-define(REG_ACC_DATA_X_LSB, 16#8). 
-define(REG_ACC_DATA_X_MSB, 16#9).
-define(REG_ACC_DATA_Y_LSB, 16#A).
-define(REG_ACC_DATA_Y_MSB, 16#B).
-define(REG_ACC_DATA_Z_LSB, 16#C).
-define(REG_ACC_DATA_Z_MSB, 16#D).

% Magnet
-define(REG_MAG_DATA_X_LSB, 16#E).
-define(REG_MAG_DATA_X_MSB, 16#F).
-define(REG_MAG_DATA_Y_LSB, 16#10).
-define(REG_MAG_DATA_Y_MSB, 16#11).
-define(REG_MAG_DATA_Z_LSB, 16#12).
-define(REG_MAG_DATA_Z_MSB, 16#13).

% Rotation
-define(REG_GYR_DATA_X_LSB, 16#14).
-define(REG_GYR_DATA_X_MSB, 16#15).
-define(REG_GYR_DATA_Y_LSB, 16#16).
-define(REG_GYR_DATA_Y_MSB, 16#17).
-define(REG_GYR_DATA_Z_LSB, 16#18).
-define(REG_GYR_DATA_Z_MSB, 16#19).

% Euler
-define(REG_EUL_HEADING_LSB, 16#1A).
-define(REG_EUL_HEADING_MSB, 16#1B).
-define(REG_EUL_ROLL_LSB, 16#1C).
-define(REG_EUL_ROLL_MSB, 16#1D).
-define(REG_EUL_PITCH_LSB, 16#1E).
-define(REG_EUL_PITCH_MSB, 16#1F).

% Linear acceleration
-define(REG_LIA_DATA_X_LSB, 16#28).
-define(REG_LIA_DATA_X_MSB, 16#29).
-define(REG_LIA_DATA_Y_LSB, 16#2A).
-define(REG_LIA_DATA_Y_MSB, 16#2B).
-define(REG_LIA_DATA_Z_LSB, 16#2C).
-define(REG_LIA_DATA_Z_MSB, 16#2D).

% Gravity Vector
-define(REG_GRV_DATA_X_LSB, 16#2E).
-define(REG_GRV_DATA_X_MSB, 16#2F).
-define(REG_GRV_DATA_Y_LSB, 16#30).
-define(REG_GRV_DATA_Y_MSB, 16#31).
-define(REG_GRV_DATA_Z_LSB, 16#32).
-define(REG_GRV_DATA_Z_MSB, 16#33).

% Temperatur
-define(REG_TEMP, 16#34).


%Register Values
-define(OPR_MODE, 2#1100). %NDOF



start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	process_flag(trap_exit, true),
	io:format("~p starting~n", [?MODULE]),
	i2c:open(?I2CBUS),
	i2c:set_slave(?I2CBUS, ?I2CADDRESS),	
	i2c_write(?REG_OPR_MODE, ?OPR_MODE),
	timer:sleep(50), %sleep 50ms to let the sensor initialise
    {ok, false}.

handle_call(update_data, _From, State) ->
	case get_imudata() of
		{ok, Data} -> {reply, Data, Data};
		{error, io, _V} -> {reply, State, State} %% @TODO Stay at old state? Or let it crash? Log!
	end;

handle_call(get_data, _From, State) ->
	case State of
		false -> {ok, Data} = get_imudata(); %% this definitely should not crash
		State -> Data = State
	end,
	{reply, Data, Data}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	io:format("~p stopping~n", [?MODULE]),
	i2c:close(?I2CBUS),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc Reads values from I2C and returns an #imudata record.
-spec get_imudata() -> {ok, _Data :: #imudata{}} | {error, _Error}.
get_imudata() ->
	try
		{ok, Gravity} = i2c_parse(vectorf, ?REG_GRV_DATA_X_LSB, 100),
		{ok, Acceleration} = i2c_parse(vectorf, ?REG_ACC_DATA_X_LSB, 100), %1 m/s 2 = 100 LSB
		{ok, Magnet} = i2c_parse(vector, ?REG_MAG_DATA_X_LSB), %We need direction only, 1 μT = 16 LSB
		{ok, Rotation} = i2c_parse(vectorf, ?REG_GYR_DATA_X_LSB, 16), %1 Dps = 16 LSB
		{ok, Linear_acceleration} = i2c_parse(vectorf, ?REG_LIA_DATA_X_LSB, 100), %1 m/s 2 = 100 LSB
		{ok, Euler} = i2c_parse(euler, ?REG_EUL_HEADING_LSB, 16), %from documentation: 1 degree = 16 LSB
		{ok, Temperature} = i2c_parse(int, ?REG_TEMP),
		{ok, #imudata{gravity = Gravity, acceleration = Acceleration,
			magnet = Magnet, rotation = Rotation,
			linear_acceleration = Linear_acceleration, euler = Euler,
			temperature = Temperature}}
	catch
		error:{badmatch, V} -> {error, io, V}
	end.

%% Helpers:

%% @doc Write one byte to the I2C Register
-spec i2c_write(_Register, _Value) -> ok | {error, _Error}.
i2c_write(Register, Value) when Register >= 0 ->
	Msg = <<Register, Value>>,
	Msgsize = byte_size(Msg),
	case i2c:rdwr(?I2CBUS,
		[#i2c_msg{ addr = ?I2CADDRESS, flags = [], len=Msgsize,
			data=Msg}])
		of
		{ok,[<<>>]} -> ok;
		{error, Error} -> {error, Error}
	end.

%% @doc Read Length bytes from the Register. 
-spec i2c_read(_Register, _Length) -> {ok, _Binary_res} | {error, _Error}.
i2c_read(Register, Length) ->
	case i2c:rdwr(?I2CBUS,
		[#i2c_msg{ addr = ?I2CADDRESS, flags = [],	len=1,
			data = <<Register>>},
		#i2c_msg{ addr = ?I2CADDRESS, flags = [rd], len=Length,
			data = <<>>}])
		of
		{ok, [<<>>, Binary_res]} -> {ok, Binary_res};
		{error, Error} -> {error, Error}
	end.

%% Getters:

%% @doc Get gravity vector.
%% @returns Vector in 100 * m/s^2
-spec i2c_parse(Type :: atom(),Start :: integer()) -> {ok, #vector_xyz{}}.
i2c_parse(vector, Start) ->
	case i2c_read(Start, 6) of
		{ok, <<X:2/little-signed-integer-unit:8,
			Y:2/little-signed-integer-unit:8,
			Z:2/little-signed-integer-unit:8>>} -> {ok, #vector_xyz{ x=X, y=Y, z=Z}};
		{error, Error} -> {error, Error}
	end;
	
i2c_parse(int, Start) ->
	case i2c_read(Start, 1) of
		{ok, <<Temp:1/little-signed-integer-unit:8>>} -> {ok, Temp};
		{error, Error} -> {error, Error}
	end.

%% @doc Vector of floats
-spec i2c_parse(Type :: atom(),Start :: integer(), Divider :: integer()) -> {ok, #vector_xyz{}}.
i2c_parse(vectorf, Start, Divider) ->
	case i2c_read(Start, 6) of
		{ok, <<X:2/little-signed-integer-unit:8,
			Y:2/little-signed-integer-unit:8,
			Z:2/little-signed-integer-unit:8>>} ->
				{ok, #vector_xyzf{ x=X/Divider, y=Y/Divider, z=Z/Divider}};
		{error, Error} -> {error, Error}
	end;

i2c_parse(euler, Start, Divider) ->
	case i2c_read(Start, 6) of
		{ok, <<X:2/little-signed-integer-unit:8,
			Y:2/little-signed-integer-unit:8,
			Z:2/little-signed-integer-unit:8>>} ->
				{ok, #euler{ heading=X/Divider, roll=Y/Divider, pitch=Z/Divider}};
		{error, Error} -> {error, Error}
	end.
