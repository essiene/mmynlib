-module(watchdog_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

init_per_group(_, Cfg) ->
    Cfg.

end_per_group(_, Cfg) ->
    Cfg.

init_per_testcase(_, Cfg) ->
    {ok, Wd} = wd:start_link(),
    timer:sleep(5000),
    [{wd, Wd}|Cfg].

end_per_testcase(_, Cfg) ->
    Wd = ?config(wd, Cfg),
    wd:stop(Wd),
    Cfg.

all() -> 
    [{group, group1}].

groups() ->
    [{group1, [parallel], [create_watchdog, all_children_loaded, child_one_running]}].

create_watchdog(_Cfg) ->
    {ok, Wd} = wd:start_link(),
    ok = wd:stop(Wd).

all_children_loaded(Cfg) ->
    Wd = ?config(wd, Cfg),
    All = watchdog:which_children(Wd),
    4 = length(All).

child_one_running(Cfg) ->
    Wd = ?config(wd, Cfg),
    {1, _, _, 1, _,_,_} = watchdog:child_info(Wd, 1).
