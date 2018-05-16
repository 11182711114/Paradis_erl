-module(index).
-behaviour(gen_server).
-compile(export_all).


-define(NAME, index).

%% Start/Stop/Init

start() ->
	{ok, Pid} = gen_server:start(?MODULE, [], []),
	(catch register(?NAME, Pid)),
	Pid.
stop() -> get_server:call(?NAME, stop).
init([]) -> {ok, maps:new()}.


%% Helpers

index_file({Msg, FileName, Md5, Size}) -> gen_server:call(?NAME, {Msg, FileName, Md5, Size}).
show_index() -> gen_server:call(?NAME, {show_index}).
filename({Msg,Md5}) -> gen_server:call(?NAME, {Msg,Md5}).


%%% Calls

handle_call({add_to_index, FileName, Md5, Size}, _From, State) ->
	NewState = State#{Md5 => {FileName, Size}},
	{reply, ok, NewState};

handle_call({show_index}, _From, State) ->
	Reply = State,
	{reply, Reply, State};

handle_call({get_filename, Md5}, _From, State) ->
	{FileName, _Size} = maps:get(Md5, State),
	{reply, FileName, State}.




%% Unused
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
code_change(_, State, _) -> {ok, State}.