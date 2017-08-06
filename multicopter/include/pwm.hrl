-define(COUNT_OF_MOTORS, 6).
-record(motorconf,
	{
		a :: float(),
		b :: float(),
		c :: float(),
		d :: float(),
		e :: float(),
		f :: float()
	}).

-record(motorstate,
	{
		motors :: #motorconf{},
		st :: atom()
	}).
