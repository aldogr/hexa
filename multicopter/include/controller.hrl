-record(pidstate, {
	kp :: float(),
	ki :: float(),
	kd :: float(),

	kp_a :: float(),
	ki_a :: float(),
	kd_a :: float(),

	t :: integer(),
	integral :: float(),
	integral_a :: float(),
	double_integral :: float(),
	last_e :: float(),
	imax :: float(),
	imin :: float(),
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
