-module(smplrest_app).
-behaviour(application).

-export([ start/2 ]).
-export([ stop/1 ]).

start(_Type, _Args) ->
	
	io:format(">>>>> ~p ~n", [application:get_all_key()] ),

	case smplrest_sup:start_link() of
		{error, Reason} -> {error, Reason};
		{ok, Pid} -> {ok, Pid}
	end.

stop(_State) ->
	ok.

