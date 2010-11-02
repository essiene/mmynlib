-module(watchdog).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-export([start_link/2, start_link/3, stop/1, which_children/1]).

-export([behaviour_info/1]).

-record(st_watchdog, {sup, args, restart, num_children, children, pidmap}).
-record(wd_child, {id, starttime, startups, total_uptime}).

behaviour_info(callbacks) ->
    [{init, 1}];
behaviour_info(_Other) ->
    undefined.


start_link(Module, Args) ->
    gen_server:start_link(?MODULE, [Module|Args], []).

start_link(Name, Module, Args) ->
    gen_server:start_link(Name, ?MODULE, [Module|Args], []).

stop(Ref) ->
    gen_server:cast(Ref, {'$watchdog', stop}).

which_children(Ref) ->
    gen_server:call(Ref, {'$watchdog', which_children}).


% The init spec is now:
% Mod:init/1 ->
% {ok, ChildSpec, WatchDogSpec}
%   ChildSpec is a normal child spec to a supervisor
%   WatchDogSpec -> 
%       {NumChildren, {MaxRestart, MaxTime}, {MinWait, MaxWait, WaitDelta}}
%       NumChildren -> integer()
%
init([Mod|Args]) ->
    process_flag(trap_exit, true),
    case Mod:init(Args) of
        {ok, {ChildId, {M,F,ChildArgs}, _Restart, Shutdown, Type, Modules},
          {NumChildren, {MaxR, MaxT}, {_Min,_Max,_Delta}=RestartSpec}} ->
            ChildSpec = {ChildId, {M,F,[]}, temporary, Shutdown, Type, Modules},
            case wd_child_sup:start_link(MaxR, MaxT, ChildSpec) of
                    ignore ->
                        ignore;
                    {error, Error} ->
                        {stop, Error};
                    {ok, Pid} ->
                        {ok, #st_watchdog{sup=Pid, args=ChildArgs, 
                           restart=RestartSpec, num_children=NumChildren, 
                           children=ets:new(ets, [private, set, 
                                   {keypos, 2}]), pidmap=[]}, 500}
           end;
        ignore ->
            ignore;
        Other ->
            {stop, {bad_return, Mod, init, Other}}
    end.

handle_call({'$watchdog', which_children}, _F, #st_watchdog{pidmap=PidMap, children=Ets}=St) ->
    ChildStats = fun(Child, Accm) -> [child_info(Child, PidMap)|Accm] end,
    Stats = ets:foldl(ChildStats, [], Ets),
    {reply, Stats, St};
handle_call(R, _F, St) ->
    {reply, {error, R}, St}.

handle_cast({'$watchdog', stop}, St) ->
    {stop, normal, St};
handle_cast(_R, St) ->
    {noreply, St}.

handle_info({start_child, Id, Cur}, St) ->
    start_child(St, Id, Cur);
handle_info(timeout, #st_watchdog{num_children=NumChildren}=St) ->
    start_all(St, NumChildren),
    {noreply, St};
handle_info({'DOWN', _, process, Pid, Reason}, #st_watchdog{restart={Min,_,_}, children=Ets, pidmap=PidMap0}=St) ->
    case lists:keytake(Pid, 1, PidMap0) of
        false ->
            error_logger:error_msg("Unwatched Child with pid ~p, died for reason: ~p~n", [Pid, Reason]),
            {noreply, St};
        {value, {Pid, Id}, PidMap1} ->
            St1 = St#st_watchdog{pidmap=PidMap1},
            case ets:lookup(Ets, Id) of
                [] ->
                    ok;
                [Child] -> 
                    child_died(Child, St1),
                    error_logger:error_msg("Child with id ~p, died for reason: ~p~n", [Id, Reason]),
                    sched_restart_child(St1, Id, Min)
            end,
            {noreply, St1}
    end;
handle_info(_R, St) ->
    {noreply, St}.

terminate(_,_) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

start_all(_, 0) ->
    ok;
start_all(#st_watchdog{restart={Min, _, _}}=St, ChildId) ->
    self() ! {start_child, ChildId, Min},
    start_all(St, ChildId-1).

start_child(#st_watchdog{sup=Sup, args=Args}=St, Id, Cur) -> 
    case wd_child_sup:start_child(Sup, [Id|Args]) of
        {ok, Pid} ->
            St1 = child_started(St, Id, Pid),
            {noreply, St1};
        {ok, Pid, _Info} ->
            St1 = child_started(St, Id, Pid),
            {noreply, St1};
        {error, Reason} ->
            error_logger:error_msg("Error: ~p while starting child ~p~n", [Reason, Id]),
            sched_restart_child(St, Id, Cur),
            {noreply, St}
    end.

child_started(#st_watchdog{children=Ets, pidmap=PidMap0}=St, Id, Pid) -> 
    PidMap1 = [{Pid, Id}|PidMap0],
    WdChild = case ets:lookup(Ets, Id) of 
        [] ->
            child_new(Id);
        [Child] -> 
            child_restarted(Child)
    end,
    ets:insert(Ets, WdChild),
    erlang:monitor(process, Pid),
    error_logger:info_msg("Started child: ~p~n", [Id]),
    St#st_watchdog{pidmap=PidMap1}.

sched_restart_child(#st_watchdog{restart={_,Max,_}}, Id, Cur) when Cur >= Max ->
    erlang:send_after(Max, self(), {start_child, Id, Max});
sched_restart_child(#st_watchdog{restart={_,_,Delta}}, Id, Cur) ->
    Next = Cur + Delta,
    erlang:send_after(Cur, self(), {start_child, Id, Next}).

child_new(Id) ->
    StartTime = erlang:now(),
    #wd_child{id=Id, starttime=StartTime, startups=1, total_uptime=0}.

child_restarted(#wd_child{startups=Startups0}=Child) ->
    StartTime = erlang:now(),
    Child#wd_child{starttime=StartTime, startups=Startups0+1}.

child_died(#wd_child{starttime=T0, total_uptime=Tup0}=Child0, #st_watchdog{children=Ets}) ->
    T1 = erlang:now(),
    Uptime = timer:now_diff(T1, T0),
    Tup1 = Tup0 + Uptime,
    Child1 = Child0#wd_child{total_uptime=Tup1},
    ets:insert(Ets, Child1).

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
