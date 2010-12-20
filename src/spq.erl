-module(spq).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).

-record(st_spq, {dets, ptime, pstruct}).
-record(pstruct, {len, list}).


init([Filename, Freq]) ->
    case dets:open_file(Filename, [{repair, force}, {type, set},
                 {keypos, 1}]) of
         {error, Reason} ->
             {stop, Reason};
         {ok, Filename} ->
             case pstruct_load(Filename) of
                 {error, Reason} ->
                     {stop, Reason};
                 {ok, Pstruct} ->
                     schedule_persistence_timer(Freq),
                     St = #st_spq{dets=Filename, ptime=Freq, pstruct=Pstruct},
                     {ok, St}
             end
     end.


handle_call(R, _F, St) ->
    {reply, {error, R}, St}.

handle_cast(_R, St) ->
    {noreply, St}.

handle_info({self(), saveq}, #st_spq{dets=Dets, pstruct=Pstruct, ptime=Freq}=St) ->
    ok = pstruct_save(Pstruct, Dets),
    schedule_persistence_timer(Freq),
    {noreply, St};
handle_info(_R, St) ->
    {noreply, St}.

terminate(_,_) ->
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


schedule_persistence_timer(Freq) ->
    Self = self(),
    erlang:send_after(Freq, Self, {Self, saveq}).

pstruct_load(Dets) ->
    case dets:lookup(Dets, pstruct) of
        {error, Reason} ->
            {error, Reason};
        [] ->
            {ok, pstruct_new()};
        [Pstruct] ->
            {ok, Pstruct}
    end.

pstruct_new() ->
    #pstruct{len=0, list=[]}.

pstruct_save(Pstruct, Dets) ->
    case dets:insert(Dets, Pstruct) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            ok
    end.
