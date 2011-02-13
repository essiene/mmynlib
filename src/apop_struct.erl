-module(apop_struct).
-export([new/1,new_req/3,len/1,schedule_timer/1,handle_timer/3]).

-record(apop_struct, {freq, q}).
-record(apop_req, {sender, count, ref}).


new(Freq) ->
    schedule_timer(Freq),
    {ok, #apop_struct{freq=Freq, q=queue:new()}}.

new_req(#apop_struct{q=Q0}=A0, S, C) ->
    Ref = make_ref(),
    Req = #apop_req{sender=S, count=C, ref=Ref},
    Q1 = queue:in(Req, Q0),
    {A0#apop_struct{q=Q1}, Ref}.

len(#apop_struct{q=Q}) ->
    queue:len(Q).


schedule_timer(Freq) ->
    Self = self(),
    erlang:send_after(Freq, Self, {Self, perform_apop}).

handle_timer(#apop_struct{freq=F, q=Q0}=A0, Fun, Accm0) ->
    case queue:out(Q0) of
        {empty, Q0} ->
            schedule_timer(F),
            {A0, Accm0};
        {{value, #apop_req{sender=S, count=C, ref=Ref}}, Q1} ->
            case is_process_alive(S) of
                true -> 
                    case catch(Fun(Accm0, S, C, Ref)) of
                        {ok, Accm1} ->
                            handle_timer(A0#apop_struct{q=Q1}, Fun, Accm1);
                        {nop, Accm1} ->
                            schedule_timer(F),
                            {A0, Accm1};
                        Other ->
                            error_logger:error_msg("Error handling APOP request: ~p~n", [Other]),
                            handle_timer(A0#apop_struct{q=Q1}, Fun, Accm0)
                    end;
                false -> 
                    ok
            end
    end.
