-module( smplrest ).

-export([ start/0, start/1 ]).
-export([ stop/0 ]).


-define( APPLICATION, ?MODULE ).

start( ) ->
	start( [?APPLICATION, sasl] ).

start( [] ) -> ok;
start( [ App|DelayList ] ) ->
	io:format("Starting ~p ~n", [ App ] ),
	case application:start( App ) of
		{error, { not_started, MissingApp } } ->
			io:format("Need ~p ~n", [ MissingApp ] ),
			start( [MissingApp,App|DelayList] );

		{error, { already_started, _AlreadyApp} } -> ok;

		_Else -> start( DelayList )
		
	end.

stop( ) ->
	application:stop( ?APPLICATION ).