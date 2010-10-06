-module(watchdog).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-export([start_link/2, start_link/3]).

-record(st_watchdog, {sup, args, restart, num_children, children}).


start_link(Module, Args) ->
    gen_server:start_link(?MODULE, [Module|Args], []).

start_link(SupName, Module, Args) ->
    gen_server:start_link(SupName, ?MODULE, [Module|Args], []).



init([Mod|Args]) ->
    case Mod:init(Args) of
        {ok, {Sup, ChildArgs, {Min, _, _}=RestartSpec, NumChildren}} ->
                {ok, #st_watchdog{sup=Sup, args=ChildArgs, restart=RestartSpec,
                        num_children=NumChildren, children=ets:new(ets, [private, set, {keypos, 1}])}, Min};
        ignore ->
            ignore
    end.

handle_call(R, _F, St) ->
    {reply, {error, R}, St}.

handle_cast(_R, St) ->
    {noreply, St}.

handle_info(_R, St) ->
    {noreply, St}.

terminate(_,_) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.
