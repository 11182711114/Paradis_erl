-module(tracker).
-behaviour(gen_server).
-compile(export_all).

-define(PORT, 43534). % Will this get code updates properly?

start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    (catch register(tracker, Pid)),
    Pid.

init(_Args) ->
    {ok, maps:new()}.

handle_call({i_am_interested_in, Md5} = Msg, {Peer, _Tag}, State) ->
    io:format("Tracker, ~p~n",[Msg]),
    L = maps:get(Md5, State, []),
    NewL = [Peer|L],
    {reply, ack, State#{Md5 => NewL}};
handle_call({who_is_interested, Md5} = Msg, _From, State) ->
    io:format("Tracker, ~p~n",[Msg]),
    L = maps:get(Md5, State, []),
    {reply, L, State};
handle_call({i_have, Md5}, {Peer, _Tag} = Msg, State) ->
    io:format("Tracker, ~p~n",[Msg]),
    L = maps:get(Md5, State, []),
    NewL = [Peer|L],
    {reply, ack, State#{Md5 => NewL}}.

handle_cast(_Msg, State) ->
    {noreply, State}.