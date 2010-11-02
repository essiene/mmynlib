-module(wd_gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(st_watchdog, {sup, args, restart, num_children, children}).



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
                           children=wd_children:new()}, 500}
           end;
        ignore ->
            ignore;
        Other ->
            {stop, {bad_return, Mod, init, Other}}
    end.

handle_call({'$watchdog', which_children}, _F, #st_watchdog{children=C}=St) ->
    {reply, wd_children:info(C), St};
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
handle_info({'DOWN', _, process, Pid, Reason}, #st_watchdog{restart={Min,_,_}, children=C0}=St) ->
    case wd_children:process_down(C0, Pid) of
        {error, pid_not_found} ->
            error_logger:error_msg("Unwatched Child with pid ~p, died for reason: ~p~n", [Pid, Reason]),
            {noreply, St};
        {C1, Id} ->
            St1 = St#st_watchdog{children=C1},
            error_logger:error_msg("Child with id ~p, died for reason: ~p~n", [Id, Reason]),
            sched_restart_child(St1, Id, Min),
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

child_started(#st_watchdog{children=C0}=St, Id, Pid) -> 
    erlang:monitor(process, Pid),
    C1= wd_children:process_up(C0, Pid, Id),
    error_logger:info_msg("Started child: ~p~n", [Id]),
    St#st_watchdog{children=C1}.

sched_restart_child(#st_watchdog{restart={_,Max,_}}, Id, Cur) when Cur >= Max ->
    erlang:send_after(Max, self(), {start_child, Id, Max});
sched_restart_child(#st_watchdog{restart={_,_,Delta}}, Id, Cur) ->
    Next = Cur + Delta,
    erlang:send_after(Cur, self(), {start_child, Id, Next}).
