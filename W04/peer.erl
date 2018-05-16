-module(peer).
-compile(export_all).


-define(TESTFILE, "files/test.txt").
-define(DOWNLOAD_LOC, "in/").


%%%%%%%%%%%%%%%%%%%%%
%%					
%% Make sure that a file exists and is referenced in the TESTFILE definition above,
%% the DOWNLOAD_LOC will be created if it does not exist
%% the resulting downloaded file will be in DOWNLOAD_LOC ++ TESTFILE,
%% i.e. with the default definitions "in/files/test.txt".
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

	add_want(P2, Md5),

	%timer:sleep(10000),
	% exit(P1),
	% exit(P2),
	% exit(tracker),
	% exit(index),
	io:format("Returning test function, the transfer will probably take longer due to wait timers to avoid request races").

start() -> spawn(?MODULE, init, []).

init() -> loop([], 1).


%% This informs the tracker the peer is interested in a file
%% Entry
send_interested(L) -> send_interested(L, []).

send_interested([], Res) -> Res;

send_interested([{Md5, not_req} | T], Res) ->
	%io:format("Sending interested~n"),
	tracker:peer_wants({i_am_interested_in, Md5, self()}),
	send_interested(T, [{Md5, req} | Res]);
	
send_interested([{_, req} = Cur | T], Res) ->
	send_interested(T, [Cur | Res]).


%% This asks for a peer list for the wanted files
%% Entry
try_download(Wants) ->
	%io:format("Wants: ~p~n", [Wants]),
	try_download(Wants, []).


try_download([], Res) -> Res;

try_download([{Md5, req} = Cur | T], Res) ->
	%io:format("Trying to download ~p~n", [Md5]),
	Peers = ask_for_peers(Md5),
	%io:format("Received peer list: ~p~n",[Peers]),
	query_peers(Peers, Md5),
	try_download(T, [Res|Cur]);

try_download([{_Md5, not_req} = Cur | T], Res) ->
	try_download(T, [Res|Cur]).


%% This asks for a peer list for a file
ask_for_peers(Md5) ->
	%io:format("Asking for peers ~p~n", [Md5]),
	tracker:who_has(Md5).

%% This asks peers what they have for a file
query_peers([], _Md5) -> continue;
query_peers([Peer | T], Md5) ->
%	io:format("Querying peer ~p for ~p~n", [Peer, Md5]),
	Response = query_peer(Peer, Md5),
	case Response of 
		{i_have, true} -> request_from_peer(Peer, Md5);
		_ -> continue
	end,
	query_peers(T, Md5).

%% This asks a peer what they have for a file
query_peer(Pid, Md5) ->
%	io:format("Querying ~p for ~p~n", [Pid, Md5]),
	rpc(Pid, {what_have_you, Md5}).

%% This requests a peer to send a file
request_from_peer(Pid, Md5) ->
%	io:format("Requesting from ~p data ~p~n", [Pid, Md5]),
	rpc(Pid, {send_me, Md5, self()}).


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


%% Wants = [{Md5, req OR not_req}]
loop(Wants, Mode) ->
	if 
		Mode == 2 ->
			SIWant = send_interested(Wants),
			try_download(SIWant),
%			io:format("~p:\tNew Want state = ~p~n",[self(), SIWant]),
			timer:sleep(500),
			loop(SIWant, 1);
		true ->
			receive
				{From, Ref, {what_have_you, Md5}} -> 
					From ! {Ref, {i_have, check_file_exists_for_md5(Md5)}},
					loop(Wants, 2);

				{From, Ref, {send_me, Md5, To}} -> 
					FileName = index:filename({get_filename, Md5}),
					send_file(To, FileName),
					From ! {Ref, file_sent},
					loop(Wants, 2);

				{From, Ref, {save, Data, _Filename}} ->
%					io:format("~p:\tReceived save req~n",[self()]),
					Md5 = save_file(Data),
					From ! {Ref, ok},
%					io:format("~p:\tRemoving {~p, req} from ~p~n",[self(), Md5, Wants]),
					NewWants = Wants -- [{Md5, req}],
%					io:format("~p:\tNewWants = ~p~n",[self(), NewWants]),
					loop(NewWants , 2);
				
				{From, Ref, {add_have, Md5}} ->
%					io:format("~p:\tSending: ~p to tracker~n", [self(),{i_have, Md5}]),
					gen_server:call(tracker, {i_have, Md5}),
					From ! {Ref, ok},
					loop(Wants, 2);

				{From, Ref, {add_want, Md5}} ->
					NewWants = [{Md5, not_req}|Wants],
%					io:format("~p: \tNew wantsState: ~p~n",[self(),NewWants]),
					From ! {Ref, ok},
					loop(NewWants, 2)
				after 500 ->
					timeout
			end
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
%	io:format("~p -> ~p -> ~p~n", [self(), Request, Pid]),
	Ref = make_ref(),
	Pid ! {self(), Ref, Request},
	receive
		{Ref, Response} -> Response
		after 3000 -> timeout
	end.