-module(wd).
-behaviour(watchdog).

-export([start_link/0, stop/1]).
-export([init/1]).

-define(MIN, 3000).
-define(MAX, 5000).
-define(DELTA, 500).
-define(NUM_CHILDREN, 4).


start_link() ->
    watchdog:start_link(?MODULE, []).

stop(Pid) ->
    watchdog:stop(Pid).

init([]) ->
    {ok,
        {wd_child, {wd_child, start_link, [foo, bar]}, temporary, 5000, worker, [wd_child]},
        {?NUM_CHILDREN, {3, 10}, {?MIN, ?MAX, ?DELTA}}
    }.


