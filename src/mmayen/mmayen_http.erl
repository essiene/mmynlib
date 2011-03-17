-module(mmayen_http).
-export([new/0, new/2, send/4]).
-include("mmayen.hrl").

new() ->
    new("localhost", 11581).

new(Host, Port) ->
    ibrowse:start(),
    #mmayen_http{host=Host, port=Port}.

send(#mmayen_http{host=Host, port=Port}, From, To, Msg) ->
    Url = io_lib:format("http://~s:~w/send?from=~s&to=~s&msg=~s", [Host, Port, From, To, Msg]),
    fetch_url(lists:flatten(Url)).

fetch_url(Url) ->
    %% url encode this before calling ibrowse:send_req/3
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _, _Response} ->
            ok;
        {ok, HttpStatus, _, _} ->
            {error, HttpStatus};
        {error, Reason} ->
            {error, Reason}
    end.

