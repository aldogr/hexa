-module(remote).
-behavior(gen_server).
-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../include/common.hrl").
-include("../include/remote.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	io:format("~p starting~n", [?MODULE]),
	process_flag(trap_exit, true),
	St = #nominal_values{pitch = 0.0, yaw = 0.0, roll_x = 0.0, roll_y = 0.0, emergency = false},
	{ok, St}.

handle_call(get_nominal_values, _From, State) ->
    {reply, State, State}.

handle_cast({set, Val}, _State) ->
	case Val#nominal_values.emergency of
		true -> gen_server:cast(?CONTROLLERMODULE, stop);
		_ -> ok
	end,
	{noreply, Val};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	io:format("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
