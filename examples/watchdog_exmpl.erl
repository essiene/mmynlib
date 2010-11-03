-module(watchdog_exmpl).
-include_lib("eunit/include/eunit.hrl").
-behaviour(watchdog).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    watchdog:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok,
        {wd_exmpl_srv, {wd_exmpl_srv, start_link, [foo,bar]}, temporary, 5000, worker, [wd_exmpl_srv]},
        {3, {3, 10}, {10000, 60000, 3000}}
    }.

