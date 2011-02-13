-module(watchdog).
-export([start_link/2, start_link/3, stop/1, which_children/1]).
-export([child_info/2, kill_child/2, exit_child/3, kill_all/1, exit_all/2]).

-export([behaviour_info/1]).
-define(SERVER, wd_gen_server).

behaviour_info(callbacks) ->
    [{init, 1}];
behaviour_info(_Other) ->
    undefined.


start_link(Module, Args) ->
    gen_server:start_link(?SERVER, [Module|Args], []).

start_link(Name, Module, Args) ->
    gen_server:start_link(Name, ?SERVER, [Module|Args], []).

stop(Ref) ->
    gen_server:cast(Ref, {'$watchdog', stop}).

which_children(Ref) ->
    gen_server:call(Ref, {'$watchdog', which_children}).

child_info(Ref, Id) ->
    gen_server:call(Ref, {'$watchdog', {child_info, Id}}).

kill_child(Ref, Id) ->
    exit_child(Ref, Id, kill).

exit_child(Ref, Id, Signal) ->
    gen_server:cast(Ref, {'$watchdog', {exit_child, Id, Signal}}).

kill_all(Ref) ->
    exit_all(Ref, kill).

exit_all(Ref, Signal) ->
    gen_server:cast(Ref, {'$watchdog', {exit_all, Signal}}).
