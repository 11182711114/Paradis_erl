-module(client).

-compile(export_all).

%% Use this, start_all, to start the processes
%% or use them manually,
%% to kill double & monitor use "exit(whereis(double), die).".
start_all() ->
    monitor:start(),
    client:start(),
    crasher:start().

start() ->
    register(client, spawn(?MODULE, client, [0])).

client(Tries) ->
    if % Try Limit
        Tries >= 10 -> io:format("~nclient reached max limit, stopping~n"), throw(try_limit); 
        true -> true
    end,
    Num = rand:uniform(50),
    case rpc(double, Num) of
        X when is_integer(X) -> client(Tries);
        timeout -> io:format("~ntimeout~n"), timer:sleep(1000), client(Tries+1)
    end.

rpc(Pid, Q) ->
    Tag = make_ref(),
    (catch Pid ! {self(), Tag, Q}), % Can fail due to sending to registered process
    receive 
        {Tag, Res} ->
            Res
        after 1000 ->
            timeout
    end.