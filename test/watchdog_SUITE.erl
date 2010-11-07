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
    [{group1, [parallel], [create_watchdog, all_children_loaded, 
    child_info, manage_child, kill_one_child]}].

create_watchdog(_Cfg) ->
    {ok, Wd} = wd:start_link(),
    ok = wd:stop(Wd).

all_children_loaded(Cfg) ->
    Wd = ?config(wd, Cfg),
    All = watchdog:which_children(Wd),
    4 = length(All).

child_info(Cfg) ->
    Wd = ?config(wd, Cfg),
    {1, _, _, 1, _,_,_} = watchdog:child_info(Wd, 1),
    {2, _, _, 1, _,_,_} = watchdog:child_info(Wd, 2),
    {3, _, _, 1, _,_,_} = watchdog:child_info(Wd, 3),
    {4, _, _, 1, _,_,_} = watchdog:child_info(Wd, 4).

manage_child(Cfg) ->
    Wd = ?config(wd, Cfg),
    {2, Pid0, _, 1, _,_,_} = watchdog:child_info(Wd, 2),
    wd_child:stop(Pid0),
    timer:sleep(100),
    {2, down, _, 1, 0,_,_} = watchdog:child_info(Wd, 2),
    timer:sleep(3000),
    {2, Pid1, _, 2, _,_,_} = watchdog:child_info(Wd, 2),
    true = is_pid(Pid1).

kill_one_child(Cfg) ->
    Wd = ?config(wd, Cfg),
    ok = watchdog:kill_child(Wd, 4),
    timer:sleep(100),
    {4, down, _, 1, 0,_,_} = watchdog:child_info(Wd, 4).
