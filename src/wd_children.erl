-module(wd_children).
-export([new/0, info/1, info/2, process_down/2, process_up/3]).
-export([child_new/1, child_restarted/1, child_died/2, child_info/2]).
-export([exit/2, exit/3]).

-record(wd_children, {pidmap, ets}).
-record(wd_child, {id, starttime, startups, total_uptime}).


new() ->
    P = [],
    T = ets:new(ets, [private, set, {keypos, 2}]),
    #wd_children{pidmap=P, ets=T}.

info(#wd_children{pidmap=P, ets=T}) ->
    StatsFun = fun(Child, Accm) -> 
            [child_info(Child, P)|Accm] 
    end,
    ets:foldl(StatsFun, [], T).

info(#wd_children{pidmap=P, ets=T}, Id) ->
    case ets:lookup(T, Id) of
        [] ->
            {error, not_found};
        [Child] ->
            child_info(Child, P)
    end.

exit(#wd_children{pidmap=P}, Signal) ->
    ExitAllFun = fun({Pid, _}) ->
            erlang:exit(Pid, Signal)
    end,
    lists:foreach(ExitAllFun, P).


exit(#wd_children{pidmap=P}, Id, Signal) ->
    case lists:keysearch(Id, 2, P) of
        false ->
            {error, not_found};
        {value, {Pid, Id}} ->
            erlang:exit(Pid, Signal)
    end.

process_down(#wd_children{pidmap=P0, ets=T}=C, Pid) ->
    case lists:keytake(Pid, 1, P0) of
        false ->
            {error, pid_not_found};
        {value, {Pid, Id}, P1} ->
            case ets:lookup(T, Id) of
                [] ->
                    ok;
                [Child] -> 
                    child_died(Child, T)
            end,
            {C#wd_children{pidmap=P1}, Id}
    end.

process_up(#wd_children{pidmap=P0, ets=T}=C, Pid, Id) ->
    P1 = [{Pid, Id}|P0],
    Child = case ets:lookup(T, Id) of 
        [] ->
            child_new(Id);
        [FoundChild] -> 
            child_restarted(FoundChild)
    end,
    ets:insert(T, Child),
    C#wd_children{pidmap=P1}.

child_new(Id) ->
    StartTime = erlang:now(),
    #wd_child{id=Id, starttime=StartTime, startups=1, total_uptime=0}.

child_restarted(#wd_child{startups=Startups0}=Child) ->
    StartTime = erlang:now(),
    Child#wd_child{starttime=StartTime, startups=Startups0+1}.

child_died(#wd_child{starttime=T0, total_uptime=Tup0}=Child0, T) ->
    T1 = erlang:now(),
    Uptime = timer:now_diff(T1, T0),
    Tup1 = Tup0 + Uptime,
    Child1 = Child0#wd_child{total_uptime=Tup1},
    ets:insert(T, Child1).

child_info(#wd_child{id=Id, starttime=T1, startups=StrtUps, total_uptime=Tup}, PidMap) ->
    {Pid, Uptime} = case lists:keysearch(Id, 2, PidMap) of
        false ->
            {down, 0};
        {value, {Pid0, Id}} ->
            Now = erlang:now(),
            Upt = timer:now_diff(Now, T1) div 1000000, %microsecs to secs
            {Pid0, Upt}
    end,
    Tup1 = Tup div 1000000 + Uptime,
    TupAvg = Tup1 div StrtUps,
    {Id, Pid, calendar:now_to_datetime(T1), StrtUps, Uptime, Tup1, TupAvg}.
