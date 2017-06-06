-module(timingtest).
-export([measure/1, measureParallel/1]).

measure(N) -> 
	Starttime = now(),
	loop(N),
	Endtime = now(),
	_Diff = (timer:now_diff(Endtime, Starttime))/(N).
	

loop(0) -> true;
loop(N) when N >= 0 ->
	case N rem 2 of %odd or even
		0 ->
			pwmtest:setMotor(a, 0.9),
			pwmtest:setMotor(b, 0.9),
			pwmtest:setMotor(c, 0.9),
			pwmtest:setMotor(d, 0.9),
			pwmtest:setMotor(e, 0.9),
			pwmtest:setMotor(f, 0.9);
		1 ->
			pwmtest:setMotor(a, 0.7),
			pwmtest:setMotor(b, 0.7),
			pwmtest:setMotor(c, 0.7),
			pwmtest:setMotor(d, 0.7),
			pwmtest:setMotor(e, 0.7),
			pwmtest:setMotor(f, 0.7)
	end,
	loop(N-1).

measureParallel(N) ->
	Starttime = now(),
	loopParallel(N),
	Endtime = now(),
	_Diff = (timer:now_diff(Endtime, Starttime))/N.


loopParallel(0) -> true;
loopParallel(N) when N >= 0 ->
	case N rem 2 of %odd or even
		0 ->
			pwmtest:setAllMotors([{a, 0.9},{b, 0.9},{c, 0.9},{d, 0.9},{e, 0.9},{f, 0.9}]);
		1 ->
			pwmtest:setAllMotors([{a, 0.7},{b, 0.7},{c, 0.7},{d, 0.7},{e, 0.7},{f, 0.7}])
	end,
	loopParallel(N-1).
