-module(tracker).

-compile(export_all).


start() ->
    {ok, Pid} = gen_server:start(?MODULE, [], []),
    Pid.

init() ->
    {ok, #{}}.

handle_call(Req, From, State) ->
    handle(From, Req, State).

handle(From, {i_am_interested_in, Md5}, State) ->
    L = maps:get(State, Md5, []),
    NewL = [From, L],
    {noreply, State#{Md5 => NewL}};
handle(From, {who_is_interested, Md5}, State) ->
    L = maps:get(State, Md5, []),
    {reply, L, State}.