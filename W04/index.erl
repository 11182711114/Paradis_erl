-module(index).
-behaviour(gen_server).
-compile(export_all).

port() -> 34243.

start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

init(_Args) ->
    {ok, Socket} = gen_udp:open(port(), [binary]),
    {ok, {Socket, #{}}}.

handle_call({add_to_index, FileName, Md5, Size}, From, State) ->
    {_Socket, Map} = State,
    {noreply, Map#{Md5 => {FileName, Size}}}.