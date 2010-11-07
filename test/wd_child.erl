-module(wd_child).
-behaviour(gen_server).

-export([start_link/3, stop/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).


-record(st_srv, {id, p1, p2}).

start_link(Id, P1, P2) ->
    gen_server:start_link(?MODULE, [Id, P1, P2], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).


init([Id, P1, P2]) ->
    error_logger:info_msg("STRT|~p|~p|~p|~p~n", [Id, self(), P1, P2]),
    {ok, #st_srv{id=Id, p1=P1, p2=P2}}.


handle_call(R, _F, St) ->
    {reply, {error, R}, St}.

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(_R, St) ->
    {noreply, St}.

handle_info(_R, St) ->
    {noreply, St}.

terminate(_,_) ->
    ok.

code_change(_, St, _) ->
    {ok, St}.
