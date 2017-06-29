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


## PIN-Configuration

Output		| Pin		| Motor/Signal
-----		|-----		|-----
epwm0A		| P9_22 	| a
epwm0B		| P9_21		| b
epwm1A		| P9_14		| c
epwm1B		| P9_16		| d
epwm2A		| P8_19		| e
epwm2B		| P8_13		| f
GND			| P9_0,1	| Common Ground
5V			| P9_4,5	| Sensor_Vin
I2C2_SDA	| P9_19		| I2C Signal
I2C2_SDC	| P9_20		| I2C Clock


[Calibrating IMU](https://www.youtube.com/watch?v=uH7iQrH3GpA)

Hexacopter diagram

      A        f
       \      /
        \    /
  b-----  ^  -----E
        /    \
       /      \
      C        d
