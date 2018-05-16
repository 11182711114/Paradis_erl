-module(peer).
-compile(export_all).

% Intervals for the peer to announce what it wants to the server and to request files from peers. In ms.
-define(ANNOUNCE_INTERVAL, 50).
-define(DOWNLOAD_INTERVAL, 50).

-define(TESTFILE, "files/test.txt").
-define(DOWNLOAD_LOC, "in/").



%%%%%%%%%%%%%%%%%%%%%
%%					
%% Make sure that a file exists and is referenced in the TESTFILE definition above,
%% the DOWNLOAD_LOC will be created if it does not exist
%% the resulting downloaded file will be in DOWNLOAD_LOC ++ TESTFILE,
%% i.e. with the default definitions "in/files/test.txt".
%%	
%%	This setup doesnt work with more than 2 clients wanting to download the same file 
%%		as the peers share download location
%%
%%%%%%%%%%%%%%%%%%%%%

test() ->
	%% Start everything
    tracker:start(),
    index:start(),

    P1 = peer:start(),
    io:format("P1 = ~p~n", [P1]),
    P2 = peer:start(),
    io:format("P2 = ~p~n", [P2]),

    Filename = (?TESTFILE),
    Md5 = md5_file(?TESTFILE),
    io:format("Md5 = ~p~n", [Md5]),

	%% Add the file to P1
	add_have(P1, Filename),

    Index = index:show_index(),
    io:format("The current index:~p~n", [Index]),

	%% Add the file as want for P2
	add_want(P2, Md5),

	io:format("Returning test function, the transfer will probably take longer due to wait timers to avoid request races~n").

start() -> spawn(?MODULE, init, []).

init() -> 
	erlang:send_after(?ANNOUNCE_INTERVAL, self(), announce),
	erlang:send_after(?DOWNLOAD_INTERVAL, self(), try_download),
	loop([]).


%% This informs the tracker the peer is interested in a file
%% Entry
send_interested(L) -> send_interested(L, []).

send_interested([], Res) -> Res;

send_interested([{Md5, not_req, DLStatus} | T], Res) ->
	tracker:peer_wants({i_am_interested_in, Md5, self()}),
	send_interested(T, [{Md5, req, DLStatus} | Res]);
	
send_interested([{_, req, _} = Cur | T], Res) ->
	send_interested(T, [Cur | Res]).


%% This asks for a peer list for the wanted files
%% Entry
try_download(Wants) -> try_download(Wants, []).


try_download([], Res) -> Res;

try_download([{_Md5, req, dl} = Cur | T], Res) ->
	try_download(T, [Res, Cur]);
try_download([{Md5, req, not_dl} = Cur | T], Res) ->
	Peers = ask_for_peers(Md5),
	case query_peers(Peers, Md5, false) of
		true 	-> try_download(T, [{Md5, req, dl}|Res]);
		false 	-> try_download(T, [Cur|Res])
	end;

%% Dont try download files we are not peers of, this doesnt really matter for this example but...
try_download([{_Md5, not_req, _} = Cur | T], Res) ->
	try_download(T, [Cur|Res]).


%% This asks for a peer list for a file
ask_for_peers(Md5) ->
	tracker:who_has(Md5).

%% This asks peers what they have for a file
% Base cases
query_peers([], _Md5, BoolSent) -> BoolSent;
query_peers(_Peers, _Md5, true) -> true;

query_peers([Peer | T], Md5, BoolSent) ->
	Response = query_peer(Peer, Md5),
	NewBoolSent = case Response of 
		{i_have, true} -> request_from_peer(Peer, Md5);
		_ -> BoolSent
	end,
	case NewBoolSent of
		true 	-> query_peers(T, Md5, true);
		X 		-> query_peers(T, Md5, X)
	end.

%% This asks a peer what they have for a file
query_peer(Pid, Md5) ->
	rpc(Pid, {what_have_you, Md5}).

%% This requests a peer to send a file
request_from_peer(Pid, Md5) ->
	case rpc(Pid, {send_me, Md5, self()}) of
		file_sent -> true;
		_ -> false
	end.


%% Adds a file to a peer, the peer will send a i_have msg to the tracker
%% Also adds the md5 => file to index
add_have(Pid, File) ->
    Md5 = md5_file(File),
    Size = filelib:file_size(File),
	index:index_file({add_to_index, File, Md5, Size}),
	rpc(Pid, {add_have, Md5}).

%% Adds a file to the wanted list for a peer
add_want(Pid, Md5) ->
	rpc(Pid, {add_want, Md5}).


%% Wants = [{Md5, req OR not_req, dl OR not_dl}]
loop(Wants) ->
	receive
		{From, Ref, {what_have_you, Md5}} -> 
			From ! {Ref, {i_have, check_file_exists_for_md5(Md5)}},
			loop(Wants);

		{From, Ref, {send_me, Md5, To}} -> 
			FileName = index:filename({get_filename, Md5}),
			From ! {Ref, file_sent},
			send_file(To, FileName),
			loop(Wants);

		{From, Ref, {save, Data, _Filename}} ->
			Md5 = save_file(Data),
			From ! {Ref, ok},
			ToRemove = [{Md5, req, dl}],
			NewWants = lists:subtract(Wants, ToRemove), 
			loop(NewWants);
		
		{From, Ref, {add_have, Md5}} ->
			gen_server:call(tracker, {i_have, Md5}),
			From ! {Ref, ok},
			loop(Wants);

		{From, Ref, {add_want, Md5}} ->
			NewWants = [{Md5, not_req, not_dl}|Wants],
			From ! {Ref, ok},
			loop(NewWants);

		%% Things that should happen periodically
		announce ->
			NewWants = send_interested(Wants),
			erlang:send_after(?ANNOUNCE_INTERVAL, self(), announce),
			loop(NewWants);

		try_download ->
			NewWants = try_download(Wants),
			erlang:send_after(?DOWNLOAD_INTERVAL, self(), try_download),
			loop(NewWants)
	end.


check_file_exists_for_md5(Md5) ->
	FileName = index:filename({get_filename, Md5}),
	case filelib:is_file(FileName) of
		true -> md5_file(FileName) == Md5;
		false -> false
	end.

%% Md5 stuff

md5(Data) -> binary_to_list(erlang:md5(Data)).

md5_file(File) ->
    {ok, Content} = file:read_file(File), 
	md5(Content).


%% Sending/Saving

send_file(To, File) ->
    {ok, Data} = file:read_file(File),
    BinName = list_to_binary(File),
    DataToSend = <<"save_file:", BinName/binary, $:, Data/binary>>,
	io:format("Sending ~p to ~p~n",[DataToSend, To]),
	rpc(To, {save, DataToSend, File}).

save_file(<<"save_file:", B/binary>>) ->
    [BinFileName, BinaryData] = binary:split(B, <<$:>>),
    SaveLocation = (?DOWNLOAD_LOC) ++ binary_to_list(BinFileName),
    filelib:ensure_dir(SaveLocation),
	io:format("Saving ~p as ~p~n",[BinaryData, BinFileName]),
    file:write_file(SaveLocation, BinaryData),
	md5_file(SaveLocation).

%% rpc

rpc(Pid, Request) ->
	Ref = make_ref(),
	Pid ! {self(), Ref, Request},
	receive
		{Ref, Response} -> Response
		after 3000 -> timeout
	end.