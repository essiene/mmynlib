-module(wd_child_sup).
-behaviour(supervisor).
-export([start_link/3, start_child/2]).
-export([init/1]).

start_link(MaxR, MaxT, ChildSpec) ->
    supervisor:start_link(?MODULE, [MaxR, MaxT, ChildSpec]).


start_child(Ref, Args) ->
    supervisor:start_child(Ref, Args).

init([MaxR, MaxT, ChildSpec]) ->
    {ok, {{simple_one_for_one, MaxR, MaxT}, [ChildSpec]}}.
