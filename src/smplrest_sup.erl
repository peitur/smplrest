-module(smplrest_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		{ 'smplrest_config',{'smplrest_config',start_link,[]}, permanent,2000,worker,['smplrest_config'] },
		{ 'sr_cwbmgr',{'sr_cwbmgr',start_link,[]}, permanent,2000,worker,['sr_cwbmgr'] }
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.
