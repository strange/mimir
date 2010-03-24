-module(mimir).
-export([start/0, stop/0]).

start() ->
    application:start(crypto),
    application:start(inets),
    application:start(mimir).

stop() ->
    application:stop(crypto),
    application:stop(inets),
    application:stop(mimir).
