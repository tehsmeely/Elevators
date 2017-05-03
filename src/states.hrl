-record(
	elevatorState, 
	{
		floor 				=	0	, %current floor number
		passengers			=	0	,  %count of passengers in lift
		floorDestinations	=	[]	,
		direction					,
		motor						,
		number						,
		parent
	}
).