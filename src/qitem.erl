-module(qitem).
-export([new/1, get_data/1, stats/1]).

-record(qitem, {id, tin, tout, data}).


new(D) ->
    N = erlang:now(),
    Id = qid(N),
    {#qitem{id=Id, tin=N, data=D}, Id}.

get_data(#qitem{data=D}=Q) ->
    {Q#qitem{tout=erlang:now()}, D}.

stats(#qitem{id=Id, tin=T1, tout=T2}) ->
    {Id, T1, T2}.


qid(N) ->
    Id0 = {node(), N},
    Id2 = erlang:term_to_binary(Id0),
    Id3 = erlang:md5(Id2),
    erlang:phash2(Id3).
