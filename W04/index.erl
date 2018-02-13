-module(index).

-compile(export_all).


start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

init() ->
    {ok, #{}}.

handle_call({add_to_index, FileName, Md5, Size}, From, State) ->
    {noreply, State#{Md5 => {FileName, Size}}}.