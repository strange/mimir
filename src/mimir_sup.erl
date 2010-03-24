-module(mimir_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
 
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
 
init([]) ->
    Processes = [
        {cache, {cache, start, []}, permanent, brutal_kill, worker, [cache]},
        {mimir_web, {mimir_web, start, []}, permanent, brutal_kill, worker, dynamic}
    ],
    {ok, {{one_for_one, 5, 60}, Processes}}.
