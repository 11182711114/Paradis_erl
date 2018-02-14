-module(tracker).
-behaviour(gen_server).
-compile(export_all).

port() -> 43534.

start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

init(_Args) ->
    {ok, Socket} = gen_udp:open(port(), [binary]),
    {ok, {Socket, #{}}}.

handle_call({i_am_interested_in, Md5}, From, State) ->
    {_Socket, Map} = State,
    L = maps:get(Map, Md5, []),
    NewL = [From, L],
    {noreply, Map#{Md5 => NewL}};
handle_call({who_is_interested, Md5}, _From, State) ->
    {_Socket, Map} = State,
    L = maps:get(Map, Md5, []),
    {reply, L, Map}.

handle_cast(_Msg, State) ->
    {noreply, State}.