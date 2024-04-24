-module(erlaws_app).

-include("erlaws.hrl").

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    ok = hackney_pool:start_pool(?HACKNEY_POOL, [{timeout, 15000}, {max_connections, 20}]),
    erlaws_app_sup:start_link().

stop(_State) ->
    ok.
