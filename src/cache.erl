-module(cache).
-behaviour(gen_server).
-export([start/0, stop/0, get/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

init(_Args) -> {ok, dict:new()}.

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() -> gen_server:cast(?MODULE, stop).

terminate(_Info, _State) -> ok.

get(Key, TTL, Fun) when is_function(Fun) ->
    gen_server:call(?MODULE, {get, Key, TTL, Fun}, 10000);
get(Key, TTL, Data) ->
    get(Key, TTL, fun() -> Data end).

handle_call({get, Key, TTL, Fun}, _From, State) ->
    {_, Now, _} = now(),
    Result = case dict:find(Key, State) of
        error -> not_found;
        {ok, {T, _}} when Now - T > TTL -> not_found;
        {ok, {_, V}} -> {found, V}
    end,
    case Result of
        {found, Value} ->
            {reply, Value, State};
        _ ->
            Value = Fun(),
            {reply, Value, dict:store(Key, {Now, Value}, State)}
    end;
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_, State) -> {noreply, State}.

handle_info (Info, State) ->
    {noreply, State}.

code_change (_OldVsn, State, _Extra) ->
    {ok, State}.
