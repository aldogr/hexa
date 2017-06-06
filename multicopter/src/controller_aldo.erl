%%%-------------------------------------------------------------
%%% @doc OTP gen_server
%%%-------------------------------------------------------------

-module(controller_aldo).
-behaviour(gen_server).        %implements the gen_server
                               
-define(SERVER, ?MODULE).    

%%--------------------------------------------------------------
%% Record & definitions
%%--------------------------------------------------------------
                               

-define(KP_R, 0.5). 
-define(KI_R, 1).
-define(KP_A, 0.5). 
-define(KI_A, 1).


-record(state, { 
 	  count,
	  
	  pidctl = { undefined, undefined, undefined, undefined },
	  motor = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0}


	 }).

     
                               
%%--------------------------------------------------------------
%% API Function Exports
%%--------------------------------------------------------------

-export([                      
  start_link/0,                
  stop/0,                      
  say_hello/0,                 
  get_count/0
	
]).  
             

%% -------------------------------------------------------------
%% gen_server Function Exports
%% -------------------------------------------------------------

-export([                      
  init/1,                      
  handle_call/3,       
  handle_cast/2,       
  handle_info/2,
  terminate/2,      
  code_change/3]).             

%% -------------------------------------------------------------
%% API Function Definitions
%% -------------------------------------------------------------

start_link() ->                
    gen_server:start_link({local, ?SERVER}, ?MODULE,[], []).  




	

%% -------------------------------------------------------------
%% gen_server Function Definitions
%% -------------------------------------------------------------

init([]) ->  



%{ok, Gpio18} = gpio:start_link(18, output),
%gpio:write(Gpio18, 0),


{ok, #state{
	count=0,
	pidctl = { 
		controller_aldo_ctrl:new(?KP_R, ?KI_R, ?KP_A, ?KI_A, 1.0, 1.0), 
		controller_aldo_ctrl:new(?KP_R, ?KI_R, ?KP_A, ?KI_A, 1.0, 1.0), 
 		controller_aldo_ctrl:new(?KP_R, ?KI_R, ?KP_A, ?KI_A, 1.0, 1.0),
		controller_aldo_ctrl:new(?KP_R, ?KI_R, ?KP_A, ?KI_A, 1.0, 1.0) 
		} 
	} 
}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                   


stop() -> gen_server:cast(?SERVER,stop).                   

say_hello() -> gen_server:cast(?SERVER, say_hello).              



get_count() -> gen_server:call(?SERVER, get_count).              
                               
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

         

handle_call(get_count, _From, #state{count=Count}) -> 
    {reply, 
     Count,                    
     #state{count=Count+1}     
    }.

handle_cast(stop, State) ->    
    {stop,                     
     normal,                   
     State                     
    };                         

handle_cast(say_hello, State) -> 
    io:format("Hello~n"),
    
    {noreply, #state{count=	State#state.count+1}    }.   
%%for testing                      



handle_info(Info, State) ->          error_logger:info_msg("~p~n", [Info]),     {noreply, State}.          

terminate(_Reason, _State) ->      error_logger:info_msg("terminating~n"), ok.                        

code_change(_OldVsn, State, _Extra) ->     {ok, State}.               

%% -------------------------------------------------------------
%% Internal Function Definitions
%% -------------------------------------------------------------

%{ component, P1 } = ctrl:update(P1-1,rot_data,angle_data).

%%%one for each angle and one for the altitude/throttle

%  motor1 = throttle + roll_adjust + pitch_adjust + yaw_adjust.

%%% one for each motor (need to check signs)



      


 
