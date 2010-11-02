-module(wd_children).
-export([new/0, info/0, process_down/2]).

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
