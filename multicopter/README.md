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
