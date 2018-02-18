-module(index).
-behaviour(gen_server).
-compile(export_all).

-define(PORT, 34243).

start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    (catch register(index, Pid)),
    Pid.

init(_Args) ->
    {ok, maps:new()}.

handle_call({add_to_index, FileName, Md5, Size} = Msg, _From, State) ->
    io:format("Index, ~p~n",[Msg]),
    {reply, ack, State#{Md5 => {FileName, Size}}};
handle_call({publish} = Msg, _From, State) ->
    io:format("Index, ~p~n",[Msg]),
    {reply, maps:to_list(State), State}.

handle_cast(_, State) ->
    {noreply, State}.