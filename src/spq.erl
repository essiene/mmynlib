-module(spq).
-behaviour(gen_server).

-export([open/1, open/2,close/1]).
-export([push/2]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).

-record(st_spq, {dets, ptime, pstruct, self}).
-record(pstruct, {len, q}).

open(Filename) ->
    open(Filename, 5000).

open(Filename, Freq) ->
    gen_server:start(?MODULE, [Filename, Freq], []).

close(Ref) ->
    gen_server:call(Ref, close).

push(Ref, Item) ->
    gen_server:call(Ref, {push, Item}).




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
                     St = #st_spq{dets=Filename, ptime=Freq, pstruct=Pstruct, self=self()},
                     {ok, St}
             end
     end.


handle_call({push, Item}, _, #st_spq{pstruct=Pstruct0}=St) ->
    Pstruct1 = pstruct_push(Pstruct0, Item),
    {reply, ok, St#st_spq{pstruct=Pstruct1}};

handle_call(close, _, #st_spq{pstruct=Pstruct, dets=Dets}=St) ->
    pstruct_save(Pstruct, Dets),
    {stop, normal, ok, St};

handle_call(R, _F, St) ->
    {reply, {error, R}, St}.

handle_cast(_R, St) ->
    {noreply, St}.

handle_info({S, saveq}, #st_spq{dets=Dets, pstruct=Pstruct, ptime=Freq, self=S}=St) ->
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
    #pstruct{len=0, q=queue:new()}.

pstruct_save(Pstruct, Dets) ->
    case dets:insert(Dets, Pstruct) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            ok
    end.

pstruct_push(#pstruct{len=Len0, q=Q0}=Pstruct0, Item) ->
    Q1 = queue:in(Item, Q0),
    Pstruct0#pstruct{len=Len0+1, q=Q1}.
