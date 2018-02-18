-module(peer).
-compile(export_all).

-define(MIN_ANNOUNCE_TIME, 5).

test() ->
    index:start(),
    tracker:start(),
    Peer = peer:start({[],0}),
    register(peer, Peer),
    Peer ! {add_file, "test", testmd5, [part1md5,part2md5], 2, ["hello", "world"]},
    timer:sleep(50),
    io:format("Index state:~p~n",[gen_server:call(index, {publish})]).


index_ip() -> "127.0.0.1".
index_port() -> 34243.
tracker_ip() -> "127.0.0.1".
tracker_port() -> 43534.

%% L = [{Md5, [PartMd5], [Peers], [Content] }]
start(L) ->
    Pid = spawn(?MODULE, peer, [L]),
    Pid.

add_to_index(FileName, Md5, Size) ->
    gen_server:call(index, {add_to_index, FileName, Md5, Size}),
    gen_server:call(tracker, {i_have, Md5}).

%% Takes a list of Files in the form {Md5, PartsMd5, _OldPeers, Content}
announce([], Res) -> Res;
announce([H|T], Res) ->
    {Md5, PartsMd5, _OldPeers, Content} = H,
    Peers = gen_server:call({who_is_interested, Md5}),
    announce(T, [{Md5, PartsMd5, Peers, Content}|Res]).


req_parts({Md5, PartsMd5, Peers, _Content} = _File, PartsAndPeers) ->

    test.

check_peers_parts({Md5, _PartsMd5, Peers, _Content} = File) ->
    io:format("check_peers_parts(~p)~n",[File]),
    PeersAndTags = ask_peers_parts(Md5, Peers, []),
    io:format("PeersAndTags:~p~n",[PeersAndTags]),
    PeersAndParts = gather_peer_parts(PeersAndTags, []),
    io:format("PeersAndParts:~p~n",[PeersAndParts]),
    PeersAndParts.

ask_peers_parts(_Md5, [], Res) -> Res;
ask_peers_parts(Md5, [H|T], Res) ->
    MyPid = self(),
    Tag = cast(H, MyPid, {what_have_you, Md5}),
    ask_peers_parts(Md5, T, [{H, Tag}|Res]).

gather_peer_parts([], Res) -> Res;
gather_peer_parts([{Peer, Tag}|T], Res) ->
    receive
        {_From, Tag, Response} ->
            gather_peer_parts(T, [{Peer, Response}|Res])
        after 5000 ->
            gather_peer_parts(T, [{Peer, timeout}|Res])
    end.

handle({what_have_you, Md5}, State) ->
    test.

peer({Files, LastAnnounce} = State) ->
    io:format("Peer state: ~p~n", [State]),

    io:format("Peer receiving~n"),
    ReceiveNewFiles = 
        receive
            {add_file, File, Md5, PartsMd5, Size, Content} ->
                io:format("Peer adding new file, ~p, ~p, ~p, ~p, ~p~n",[File, Md5, PartsMd5, Size, Content]),
                add_to_index(File, Md5, Size),
                L = [{Md5, PartsMd5, [], Content}|Files],
                L;
            {what_have_you, Md5} ->
                handle({what_have_you, Md5}, State)
            after 5000 ->
                Files
        end,
    %% Announcing
    io:format("Peer should announce? ~p~n",[(LastAnnounce - erlang:system_time(second)) >= ?MIN_ANNOUNCE_TIME]),
    case (LastAnnounce - erlang:system_time(second)) >= ?MIN_ANNOUNCE_TIME of
        true ->
            io:format("Peer announcing~n"),
            AnnounceNewFiles = announce(ReceiveNewFiles, []),
            NewLastAnnounce = erlang:system_time(second),
            peer({AnnounceNewFiles, NewLastAnnounce});
        false ->
            io:format("Peer not announcing~n"),
            peer({ReceiveNewFiles, LastAnnounce})
    end.

call(Pid, Query) ->
    Tag = make_ref(),
    Pid ! {self(), Tag, Query},
    receive
        {Tag, Res} ->
            Res
    end.

cast(Pid, From, Query) ->
    Tag = make_ref(),
    Pid ! {From, Tag, Query},
    Tag.