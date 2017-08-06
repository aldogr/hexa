Dynamic Software Update on a Multicopter using Erlang
=====

An OTP application

It uses [Rebar3 Profiles](https://www.rebar3.org/v3/docs/profiles) in
order to change the controll algorithm at compile time, maybe dynamic
relinking could be an option in the future.

Build with Aldo's controller
-----

    $ rebar3 as aldo compile

Build with Kilian's controller
-----

	$ rebar3 as kilian compile

## Configuration

Place all overwriting configuration inside ```config/sys.config.own```. It is ignored by git.


[Calibrating IMU](https://www.youtube.com/watch?v=uH7iQrH3GpA)
-----

Hexacopter diagram
-----

A at the front left, counterclockwise and motor display:

	$ A = S#motorconf.c - 0.5 * roll + 0.866 * pitch + yaw,
	$ B = S#motorconf.b - 1 * roll + 0 * pitch - yaw,
	$ C = S#motorconf.f - 0.5 * roll - 0.866 * pitch - yaw,
	$ D = S#motorconf.e + 0.5 * roll - 0.866 * pitch - yaw,
	$ E = S#motorconf.a + 1 * roll + 0 * pitch + yaw,
	$ F = S#motorconf.e + 0.5 * roll + 0.866 * pitch + yaw,



Finite State machine & instructions
-----



			     <- Land  (motors to minimun)
	    init(stop) [0]   <- Reset (motors to 0)
		|
		v
	    Calibrate [2]
		|
		v
	      Armed [3]
		|
		v
		     ->
	      Auto[]    Manual[]
		     <-

________________________

Land and Reset are also States, the jump from and to states init, Calibrate, Armed, Auto and Manual, is with the command[Synchorous]:

	$ state_machine:next().

and to Land (one is for Asynchronous and the other for Synchronous):

	$ state_machine:land().
	$ state_machine:sync_land().

and to Reset:

	$ state_machine:reset().
	$ state_machine:sync_reset().


States:

Init(stop): Software initialised, but not Armed, motors stop 
Calibrate: Calibrate the motors

Armed: The Hexacopter is ready to fly

Auto: Automatic mode

Manual: Manual mode

Land: Emergency landing, motors to minimun throttle (controller up or down?)

Reset: stop motors

