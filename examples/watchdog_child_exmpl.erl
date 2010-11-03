-module(watchdog_child_exmpl).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).
-export([start_link/3]).

-record(st_srv, {id, p1, p2}).


start_link(Id, P1, P2) ->
    gen_server:start_link(?MODULE, [Id, P1, P2], []).



init([Id, P1, P2]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("Client ~p/~p started [~p,~p] ~n", [Id, self(), P1, P2]),
    {ok, #st_srv{id=Id}}.

handle_call(R, _F, St) ->
    {reply, {error, R}, St}.

handle_cast(stop, St) ->
    {stop, normal, St};
handle_cast(_R, St) ->
    {noreply, St}.

handle_info(_R, St) ->
    {noreply, St}.

terminate(Reason,#st_srv{id=Id}) ->
    error_logger:info_msg("Client ~p/~p terminating: ~p~n", [Id, self(), Reason]),
    ok.

code_change(_, St, _) ->
    {ok, St}.
