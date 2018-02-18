%%% Modified from https://rosettacode.org/wiki/Echo_server#Erlang
-module(tcp_server).
-compile(export_all).
 
-define(PORT, 8085).


start(Callback) ->
    {ok, State} = apply(Callback, init, []),
    spawn(fun () -> {ok, Socket} = gen_tcp:listen(?PORT, [{packet, line}]),
                    loop(Socket, Callback, State)
          end).
 
loop(Socket, Callback, State) ->
    {ok, Conn} = gen_tcp:accept(Socket),
    io:format("Got connection: ~p~n", [Conn]),
    NewState = tcp_server:handle(Conn, Callback, State),
    loop(Socket, Callback, NewState).
 
handle(Conn, Callback, State) ->
    receive
        {tcp, Conn, Data} ->
            case apply(Callback, handle, [Data, State]) of
                {reply, Resp, NewState} ->
                    gen_tcp:send(Conn, Resp),
                    handle(Conn, Callback, NewState);
                {noreply, NewState} ->
                    NewState,
                    gen_tcp:close(Conn)
            end,
            handle(Conn, Callback, State);
        {tcp_closed, Conn} ->
            io:format("Connection closed: ~p~n", [Conn])
    end.
