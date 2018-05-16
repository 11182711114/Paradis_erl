-module(tracker).
-behaviour(gen_server).
-compile(export_all).


-define(NAME, tracker).

%% Start/Stop/Init

start() ->
	{ok, Pid} = gen_server:start(?MODULE, [], []),
	(catch register(?NAME, Pid)),
	Pid.
stop() -> get_server:call(?MODULE, stop).
init([]) -> {ok, maps:new()}.


%% Helpers

peer_wants({Msg,Md5,Peer}) -> gen_server:call(?NAME, {Msg,Md5,Peer}).
peer_has({Msg,Md5}) -> gen_server:call(?NAME, {Msg,Md5}).

who_has(Md5) -> gen_server:call(?NAME, {who_is_interested, Md5}).


%% Calls

handle_call({i_am_interested_in, Md5, Peer}, _From, State) ->
	OldPeerList = maps:get(Md5, State, []),
	NewPeerList = lists:append(OldPeerList, [Peer]),
	NewState = State#{Md5 => NewPeerList},
	{reply, ok, NewState};

handle_call({i_have, Md5}, {From, _Ref}, State) -> 
	Peers = maps:get(Md5, State, []),
	NewState = State#{Md5 => [From | Peers]},
	{reply, ok, NewState};

handle_call({who_is_interested, Md5}, _From, State) ->
	Reply = maps:get(Md5, State),
%	io:format("Tracker:\tReturning peers for ~p: ~p~n",[Md5, Reply]),
	{reply, Reply, State};

handle_call(X, From, _State) -> 
	io:format("Received unknown: ~p from ~p~n",[X, From]).


%% Unused reqs for gen_server behaviour
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
code_change(_, State, _) -> {ok, State}.