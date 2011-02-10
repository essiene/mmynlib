-module(spq).
-behaviour(gen_server).

-export([open/1, open/2,open/3, close/1]).
-export([push/2, len/1, pop/1, pop/2, apop/2, ping/1]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
         terminate/2,code_change/3]).

-record(st_spq, {dets, ptime, pstruct, self, apop_struct}).
-record(apop_struct, {freq, q}).
-record(apop_req, {sender, count, ref}).

open(Filename) ->
    open(Filename, 5000).

open(Filename, Freq) ->
    gen_server:start(?MODULE, [Filename, Freq, Freq], []).

open(Filename, Freq, ApopFreq) ->
    gen_server:start(?MODULE, [Filename, Freq, ApopFreq], []).

close(Ref) ->
    gen_server:call(Ref, close).

push(Ref, Item) ->
    gen_server:call(Ref, {push, Item}).

ping(Ref) ->
    gen_server:call(Ref, ping).

len(Ref) ->
    gen_server:call(Ref, len).

pop(Ref) ->
    gen_server:call(Ref, pop).

pop(Ref, Count) ->
    gen_server:call(Ref, {pop, Count}).

apop(Ref, Count) ->
    gen_server:call(Ref, {apop, self(), Count}).




init([Filename, Freq, ApopSvcFreq]) ->
    case dets:open_file(Filename, [{repair, force}, {type, set},
                 {keypos, 1}]) of
         {error, Reason} ->
             {stop, Reason};
         {ok, Filename} ->
             case spq_pstruct:load(Filename) of
                 {error, Reason} ->
                     {stop, Reason};
                 {ok, Pstruct} ->
                     schedule_persistence_timer(Freq),
                     case apop_struct_new(ApopSvcFreq) of
                         {error, Reason} ->
                             {stop, Reason};
                         {ok, ApopStruct} ->
                             St = #st_spq{dets=Filename, ptime=Freq, pstruct=Pstruct, 
                                          self=self(), apop_struct=ApopStruct},
                             {ok, St}
                     end
             end
     end.

handle_call(ping, _, #st_spq{pstruct=P, apop_struct=A}=St) ->
    Qlen = spq_pstruct:len(P),
    ApopLen = apop_struct_len(A),
    Reply = [{qlen, Qlen}, {apop_req_q, ApopLen}],
    {reply, Reply, St};

handle_call({apop, Sender, Count}, _, #st_spq{apop_struct=A0}=St) ->
    {A1, Ref} = apop_struct_new_req(A0, Sender, Count),
    {reply, {ok, Ref}, St#st_spq{apop_struct=A1}};

handle_call({pop, Count}, _, #st_spq{pstruct=P0}=St) ->
    {P1, Result} = spq_pstruct:pop(P0, Count),
    {reply, Result, St#st_spq{pstruct=P1}};

handle_call(pop, _, #st_spq{pstruct=P0}=St) ->
    {P1, Result} = spq_pstruct:pop(P0),
    {reply, Result, St#st_spq{pstruct=P1}};

handle_call(len, _, #st_spq{pstruct=P}=St) ->
    {reply, spq_pstruct:len(P), St};

handle_call({push, Item}, _, #st_spq{pstruct=Pstruct0}=St) ->
    Pstruct1 = spq_pstruct:push(Pstruct0, Item),
    {reply, ok, St#st_spq{pstruct=Pstruct1}};

handle_call(close, _, #st_spq{pstruct=Pstruct, dets=Dets}=St) ->
    spq_pstruct:save(Pstruct, Dets),
    {stop, normal, ok, St};

handle_call(R, _F, St) ->
    {reply, {error, R}, St}.

handle_cast(_R, St) ->
    {noreply, St}.

handle_info({S, perform_apop}, #st_spq{pstruct=P0, self=S, apop_struct=A0}=St) ->
    Fun = fun(Pstruct0, Sender, Count, Ref) ->
            case spq_pstruct:pop(Pstruct0, Count) of
                {Pstruct1, []} ->
                    {nop, Pstruct1};
                {Pstruct1, Items} -> 
                    Sender ! {Ref, qdata, Items}, 
                    {ok, Pstruct1}
            end
    end,
    {A1, P1} = handle_apop_timer(A0, Fun, P0),
    {noreply, St#st_spq{apop_struct=A1, pstruct=P1}};

handle_info({S, saveq}, #st_spq{dets=Dets, pstruct=Pstruct, ptime=Freq, self=S}=St) ->
    ok = spq_pstruct:save(Pstruct, Dets),
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



apop_struct_new(Freq) ->
    schedule_apop_timer(Freq),
    {ok, #apop_struct{freq=Freq, q=queue:new()}}.

apop_struct_new_req(#apop_struct{q=Q0}=A0, S, C) ->
    Ref = make_ref(),
    Req = #apop_req{sender=S, count=C, ref=Ref},
    Q1 = queue:in(Req, Q0),
    {A0#apop_struct{q=Q1}, Ref}.

apop_struct_len(#apop_struct{q=Q}) ->
    queue:len(Q).


schedule_apop_timer(Freq) ->
    Self = self(),
    erlang:send_after(Freq, Self, {Self, perform_apop}).

handle_apop_timer(#apop_struct{freq=F, q=Q0}=A0, Fun, Accm0) ->
    case queue:out(Q0) of
        {empty, Q0} ->
            schedule_apop_timer(F),
            {A0, Accm0};
        {{value, #apop_req{sender=S, count=C, ref=Ref}}, Q1} ->
            case is_process_alive(S) of
                true -> 
                    case catch(Fun(Accm0, S, C, Ref)) of
                        {ok, Accm1} ->
                            handle_apop_timer(A0#apop_struct{q=Q1}, Fun, Accm1);
                        {nop, Accm1} ->
                            schedule_apop_timer(F),
                            {A0, Accm1};
                        Other ->
                            error_logger:error_msg("Error handling APOP request: ~p~n", [Other]),
                            handle_apop_timer(A0#apop_struct{q=Q1}, Fun, Accm0)
                    end;
                false -> 
                    ok
            end
    end.
