-module(spq_pstruct).
-export([load/1, new/0,
        sanitize/1, save/2,
        push/2, len/1,
        pop/1, pop/2]).

        

-record(struct, {len, q}).




load(Dets) ->
    case dets:lookup(Dets, struct) of
        {error, Reason} ->
            {error, Reason};
        [] ->
            {ok, new()};
        [Pstruct] ->
            {ok, sanitize(Pstruct)}
    end.

new() ->
    #struct{len=0, q=queue:new()}.

sanitize(#struct{q=Q}) ->
    #struct{len=queue:len(Q), q=Q}.

save(Pstruct, Dets) ->
    case dets:insert(Dets, Pstruct) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            ok
    end.

push(#struct{len=Len0, q=Q0}=Pstruct0, Item) ->
    {Qitem, Id} = qitem:new(Item),
    Q1 = queue:in(Qitem, Q0),
    {Pstruct0#struct{len=Len0+1, q=Q1}, Id}.

len(#struct{len=Len}) ->
    Len.

pop(#struct{len=L0, q=Q0}=P0) ->
    try queue:get(Q0) of
        Qitem0 ->
            {_, Item} = qitem:get_data(Qitem0),
            Q1 = queue:drop(Q0),
            {P0#struct{len=L0-1, q=Q1}, {value, Item}}
     catch 
         error: empty ->
            {P0, {error, empty}}
    end.

pop(P, Count) ->
    pop(P, Count, []).

pop(P, 0, Accm) ->
    {P, lists:reverse(Accm)};
pop(P0, Count, Accm) ->
    case pop(P0) of
        {P0, {error, empty}} ->
            {P0, lists:reverse(Accm)};
        {P1, {value, Item}} ->
            pop(P1, Count - 1, [Item|Accm])
    end.


