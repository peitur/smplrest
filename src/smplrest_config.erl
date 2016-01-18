-module( smplrest_config ).
-behaviour( gen_server ).

-define( CONF_FILE, "smplrest.config" ).
-define( CONF_PATH, "priv" ).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1, stop/0, stop/1] ).
-export( [get_value/1, get_value/2, get_value/3, set_value/3, load_file/1, reload_file/0, apply_config/1] ).




start_link( ) ->
	start_link( [] ).

start_link( Config ) -> 
	gen_server:start_link( {local, ?MODULE}, ?MODULE, [Config], [] ).


stop() ->
	stop( normal ).

stop( Reason ) ->
	gen_server:call( ?MODULE, {stop, Reason} ).


get_value( Class ) ->
	gen_server:call( ?MODULE, {get, Class, all } ).

get_value( Class, Key ) -> 
	gen_server:call( ?MODULE, {get, Class, Key } ).

get_value( Class, Key, Default ) ->
	case gen_server:call( ?MODULE, {get, Class, Key } ) of
		{error, _ } -> {ok, Default};
		Else -> Else
	end.

set_value( Class, Key, Val ) -> 
	gen_server:call( ?MODULE, {set, Class, Key, Val } ).

load_file( Filename ) ->
	gen_server:call( ?MODULE, {load, config, Filename} ).

reload_file( ) ->
	gen_server:call( ?MODULE, {load, config} ).

apply_config( Config ) ->
	gen_server:call( ?MODULE, {set, apply, Config } ).

-record( state, { filename, path, app_home, config, refresh = 0 } ).

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
init( [] ) ->
	{ok, #state{ filename = ?CONF_FILE, path = ?CONF_PATH, app_home = file:get_cwd() } };

init([Config]) ->
	case i_apply_config( Config ) of
		{error, Reason} -> {stop, Reason}; 
		NewState -> {ok, NewState }
	end.


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
handle_call( {get, Class, all }, _From, #state{ config = Config } = State ) ->
	case proplists:get_value( Class, Config, undefined ) of
		undefined -> {reply, {error, missing_class}, State};
		Val -> {reply, { ok, Val }, State }
	end;

handle_call( {get, Class, Key }, _From, #state{ config = Config } = State ) ->
	case proplists:get_value( Class, Config, undefined ) of
		undefined -> {reply, {error, missing_class}, State};
		ClassVal -> 
			case proplists:get_value( Key, ClassVal, undefined ) of
				undefined -> {reply, {error, missing_key }, State};
				Val -> {reply, { ok, Val }, State }
			end
	end;


handle_call( {set, _Class, _Key, _Val}, _From, State ) ->
	{reply, {ok, undefined}, State};	

handle_call( {set, config, Config}, _From, State ) ->
	
	case i_apply_config( Config ) of
		{ error, Reason } -> {reply, {error, Reason }, State};
		NewState ->
			{reply, {ok, config}, NewState }
	end;

handle_call( {load, config}, _From, #state{ filename = Filename, path = Path, app_home = Home } = State ) ->
	FullFilename = Home+"/"+Path+"/"+Filename,

	case i_load_file( FullFilename ) of
		{error, Reason} -> 
			error_logger:error_msg( "", [] ),
			{reply, {error, Reason}, State };
		NewState -> {reply, ok, NewState }
	end;

handle_call( {load, config, Filename}, _From, State ) ->
	case i_load_file( Filename ) of
		{error, Reason} -> 
			error_logger:error_msg( "", [] ),
			{reply, {error, Reason}, State };
		NewState -> {reply, ok, NewState }
	end;

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


i_load_file( Filename ) ->
	case file:consult( Filename ) of
		{error, Reason} -> {error, Reason};
		Config -> i_apply_config( Config )
	end.

i_apply_config( Config ) ->

	case proplists:get_value( global, Config, undefined ) of
		undefined ->  {stop, {error, undefined, global }};
		GlobalConfig ->
			Filename = proplists:get_value( filename, GlobalConfig, ?CONF_FILE ),
			Filepath = proplists:get_value( path, GlobalConfig, ?CONF_PATH ),
			Referesh = proplists:get_value( refresh, GlobalConfig, 0 ),

			case proplists:get_value( app_home, GlobalConfig, undefined ) of
				undefined -> {error, { undefined, home }};
				Home -> 
					case file:set_cwd( Home ) of
						ok -> #state{ filename = Filename, path = Filepath, refresh = Referesh, app_home = Home, config = Config };
						{error, Reason} -> {error, Reason}
					end
			end
		end.
