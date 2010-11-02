-module(watchdog).
-export([start_link/2, start_link/3, stop/1, which_children/1]).

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

