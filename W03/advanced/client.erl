-module(client).

-compile(export_all).

start() ->
    register(client, spawn(?MODULE, client, [0])).

client(Tries) ->
    if 
        Tries >= 10 -> throw(try_limit);
        true -> true
    end,
    io:format("Tries: ~p~n",[Tries]),
    Num = rand:uniform(50),
    case rpc(double, Num) of
        X when is_integer(X) -> io:format("success: ~p~n",[X]), client(Tries);
        timeout -> io:format("timeout"), timer:sleep(1000), client(Tries+1)
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