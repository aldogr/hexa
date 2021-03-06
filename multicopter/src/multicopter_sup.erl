%%%-------------------------------------------------------------------
%% @doc multicopter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(multicopter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include("../include/common.hrl").

%%
%%
%%

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% @TODO Check restart and timeout values!
init([]) -> 
    {ok, { {one_for_all, 3, 5}, 
		[{pwmproc, {pwm, start_link, []},
		permanent,
		10000,
		worker,
		[pwm]},
		
		{imu, {imu_bno055, start_link, []},
		permanent,
		10000,
		worker,
		[imu_bno055]},
		
		{remote, {remote, start_link, []},
		permanent,
		10000,
		worker,
		[remote]},

		{controller, {?CONTROLLERMODULE, start_link, []},
		permanent,
		10000,
		worker,
		[controller]},
			
		{connector, {connector, start_link, []},
		permanent,
		10000,
		worker,
		[connector]},
	
		{hc_sr04, {hc_sr04, start_link, []},
		permanent,
		10000,
		worker,
		[hc_sr04]},

		{state_machine, {state_machine, start_link, []},
		permanent,
                10000,
                worker,
                [connector]}

		]} }.

%%====================================================================
%% Internal functions
%%====================================================================
