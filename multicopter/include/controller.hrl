-record(pidstate, {
	kp :: float(),
	ki :: float(),
	kd :: float(),
	t :: integer(),
	integral :: float(),
	double_integral :: float(),
	last_e :: float(),
	y :: float()
}).

-record(controllerstate, {
	timer :: reference(),
	pid_pitch :: #pidstate{},
	pid_yaw :: #pidstate{},
	pid_rollx :: #pidstate{},
	pid_rolly :: #pidstate{},
	motors :: #motorconf{}
}).
