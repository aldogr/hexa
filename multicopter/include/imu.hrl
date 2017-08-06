-record(vector_xyz, {
	x :: integer(),
	y :: integer(),
	z :: integer()
}).

-record(vector_xyzf, {
	x :: float(),
	y :: float(),
	z :: float()
}).

-record(euler, {
	heading :: float(),
	roll :: float(),
	pitch :: float()
}).

-record(quaternion, {
	w :: integer(),
	x :: integer(),
	y :: integer(),
	z :: integer()
}).

-record(imudata, {
	gravity ::  #vector_xyzf{}, 
	acceleration :: #vector_xyzf{},
	magnet :: #vector_xyz{}, %int is enough, we just need direction
	rotation :: #vector_xyzf{},
	linear_acceleration :: #vector_xyzf{},
	temperature :: integer(),
	euler :: #euler{}
	%quaternion :: #quaternion{}
}).
