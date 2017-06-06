%%%----------------------------------------------------------------------------
%%% @doc P control
%%%----------------------------------------------------------------------------

-module(controller_aldo_ctrl).         

-define(Ts, 0.001).   %% time it takes in a full loop x 2 of the response rate of the sys (x3 or 5 better)

%% in seconds

-export([new/6, hello/4]).

-export([update/3]).


                               
-record(pid,          
	{ kp_ro, ki_ro, kp_an, ki_an,   %Control values for rotation and angle

	  int_ro = 0,    %% Rotation Integrator acumulated (the old)
	  int_an = 0,    %% Angle Integrator acumulated (the old)

	imax = 10, imin = -10,   %%%%%first check with the same limits

	  set_point,       %% requested Angle position

	  curr_ro,	   %% current gyro position
	  curr_an	   %% current angle position

	  
	  
	}).




%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------
hello(Kpro,Kiro,Kpan,Kian) ->
	io:format("Hello~n").
	

new(Kpro,Kiro,Kpan,Kian,Iro,Ian)
  when is_number(Kpro), is_number(Kiro), is_number(Kpan),
       is_number(Kian), is_number(Iro), is_number(Ian) ->
    new_(Kpro,Kiro,Kpan,Kian,Iro,Ian).

new_(Kpro,Kiro,Kpan,Kian,Iro,Ian) ->
	  
    #pid { kp_ro = Kpro, ki_ro = Kiro,
	   kp_an = Kpan, ki_an = Kian,  

	   int_ro = 0,    
	   int_an = 0,

	   set_point = 0,

	   curr_ro = 0,	   
	  curr_an = 0
	   
	    }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  			%%% Feedro: rotational feed (actual rotation)
			%%% Feedan: angle feed (actual angle)
update(PID, Feedro,Feedan) when is_number(Feedro), is_number(Feedan) ->
    update_ef(PID, PID#pid.set_point, Feedro, Feedan).    

update_ef(PID, SetPoint, Feedro, Feedan) ->

	%%%%%%%%%%%%%%%%%%%% Angle Control
	Error_an = SetPoint - Feedan,

	I0 = PID#pid.int_an,  %the old integral    
	I1 = I0 + Error_an*?Ts,		% the new integral  
                 
	I2 = max(min(PID#pid.imax,I1),PID#pid.imin),  %%%antiwindup

	Rot_rate = (PID#pid.kp_an*Error_an)+(PID#pid.ki_an*I2),  %%desired rotational rate
    
	%%%%%%%%%%%%%%%%%%% Rotation Gyro Control -- Feeds from the angle
	Error_ro = Rot_rate - Feedro,

	I0r = PID#pid.int_ro,  %the old integral    
	I1r = I0r + Error_ro*?Ts,		% the new integral  
                 
	I2r = max(min(PID#pid.imax,I1r),PID#pid.imin),  %%%antiwindup

	Output = (PID#pid.kp_ro*Error_ro)+(PID#pid.ki_ro*I2r),  
	   



      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 {Output,PID#pid { int_ro=I2r, int_an =I2} }.   

 %% returns the component for the motors, and updates both integrals to reuse them next time




