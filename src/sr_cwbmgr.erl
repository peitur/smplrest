-module( sr_cwbmgr ).
-behaviour( gen_server ).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-define( CONFIG, smplrest_config ).
-define( DEFAULT_PORT, 8080 ).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, start_link/1, stop/0, stop/1] ).

start_link( ) ->
	start_link( [] ).

start_link( Config ) -> 
	gen_server:start_link( {local, ?MODULE}, ?MODULE, [Config], [] ).


stop() ->
	stop( normal ).

stop( Reason ) ->
	gen_server:call( ?MODULE, {stop, Reason} ).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, { port, dispatch }).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init( _ ) ->
	erlang:process_flag( trap_exit, true ),
	Port = ?CONFIG:get_value( rest, port ),
	Dispatch = undefined,

    {ok, #state{ port = Port, dispatch = Dispatch }, 1000 }.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call( {stop, Reason} , _From, State ) ->
	{stop, Reason, State };

handle_call( Request, _From, State) ->
    {reply, {stop, {undefined, Request}}, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast( _Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info( timeout, #state{ port = Port, dispatch = Disp } = State ) ->
	case Disp of
		undefined ->
			io:format( "Starting service on port ~p ~n", [ Port ] ),
			Dispatch = cowboy_router:compile([
				{'_', [
					{"/", sr_http_handler, []}
				]}
			]),
			{ok, _} = cowboy:start_http(http, 100, [ { port, i_port_fix(Port) }], [
				{env, [{dispatch, Dispatch}]}
			]),
			
%			sr_http_handler_sup:start_link( [] )

			{noreply, State#state{ dispatch = Dispatch } };

		_Else ->
			{noreply, State}
	end;

handle_info( _Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate( _Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change( _OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

i_port_fix( Port ) when is_integer( Port ) -> Port;
i_port_fix( Port ) when is_list( Port ) -> list_to_integer( Port );
i_port_fix( _ ) -> ?DEFAULT_PORT.	
