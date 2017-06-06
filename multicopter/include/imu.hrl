-record(vector_xyz, {
	x :: integer(),
	y :: integer(),
	z :: integer()
}).

-record(euler, {
	heading :: integer(),
	roll :: integer(),
	pitch :: integer()
}).

-record(quaternion, {
	w :: integer(),
	x :: integer(),
	y :: integer(),
	z :: integer()
}).

-record(imudata, {
	gravity ::  #vector_xyz{},
	acceleration :: #vector_xyz{},
	magnet :: #vector_xyz{},
	rotation :: #vector_xyz{},
	linear_acceleration :: #vector_xyz{},
	temperature :: integer(),
	euler :: #euler{},
	quaternion :: #quaternion{}
}).
