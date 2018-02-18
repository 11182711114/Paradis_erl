-module(peer2).
-compile(export_all).

-define(MIN_TRACKER_ANNOUNCE_INTERVAL, 30).
-define(MIN_PEER_ASK_INTERVAL, 15).

test() ->
    index:start(),
    tracker:start(),
    Peer = peer2:start(),
    Peer ! {add_file, "test", testmd5, [part1md5,part2md5], 2, ["hello", "world"]},
    Peer2 = peer2:start(),
    Peer2 ! {add_want, testmd5, [part1md5,part2md5]},
    timer:sleep(50),
    io:format("Index state:~p~n",[gen_server:call(index, {publish})]).
    
%% L = [{Md5, [PartMd5], [Peers], [Content] }]
start() ->
    Pid = spawn(?MODULE, peer, [{[],[],0,0}]),
    Pid.

add_to_index(FileName, Md5, Size) ->
    gen_server:call(index, {add_to_index, FileName, Md5, Size}),
    gen_server:call(tracker, {i_have, Md5}).

%% Takes a list of Files in the form {Md5, PartsMd5, _OldPeers, Content}
announce([], Res) -> Res;
announce([H|T], Res) ->
    {Md5, _PeerParts} = H,
    Peers = gen_server:call(tracker, {who_is_interested, Md5}),
    announce(T, [{Md5, [{Peer, []} || Peer <- Peers]}|Res]).

ask_peers_what_parts([], []) -> no_peers;
ask_peers_what_parts({_Md5, []}, Peers) ->
    io:format("ask_peers_what_parts_base"),
    gather_peer_parts(Peers, []);
ask_peers_what_parts({Md5, [H|T]} = Peer_Files, Peers) ->
    io:format("ask_peers_what_parts: ~p, ~p~n",[Peer_Files, Peers]),
    {Peer, _Parts} = H,
    cast(Peer, {what_have_you, Md5}),
    ask_peers_what_parts({Md5, T}, [Peer|Peers]).

gather_peer_parts([H|T] = L, Res) ->
    io:format("gather_peer_parts: ~p, ~p~n",[L, Res]),
    receive
        {H, Parts} ->
            gather_peer_parts(T,[{H, Parts}|Res])
        after 1000 ->
            gather_peer_parts(T,[{H, []}|Res])
    end.
            
req_parts(Peer_Files, File_Desc) ->
    tmp.

% Peer_Files = [{Md5, [{Peer, Parts}]}]
% File_Desc = [{Md5, PartsMd5, Content}]
% LastAnnounce = int, Last time announced to tracker
peer({Peer_Files, File_Desc, LastAnnounce, LastPeerAsk} = State) ->
    io:format("Peer state: ~p~n", [State]),
    case ask_peers_what_parts(Peer_Files, []) of
        no_peers ->
            cont;
        X ->
            req_parts(Peer_Files, File_Desc)
    end,
        

    %% Receiving commands/Queries from other peers
    io:format("Peer receiving~n"),
    NewState = 
        receive
            %% USER COMMANDS
            {add_want, Md5, PartsMd5} ->
                {Peer_Files, [{Md5, PartsMd5, []}], LastAnnounce, LastPeerAsk};
            {add_file, File, Md5, PartsMd5, Size, Content} ->
                io:format("Peer adding new file, ~p, ~p, ~p, ~p, ~p~n",[File, Md5, PartsMd5, Size, Content]),
                add_to_index(File, Md5, Size),
                New_File_Desc = [{Md5, PartsMd5, Content}|File_Desc],
                New_Peer_Files = [{Md5, []}],
                {New_Peer_Files, New_File_Desc, LastAnnounce, LastPeerAsk};

            %% PEER REQUESTS
            {From, {what_have_you, Md5}} = Msg ->
                io:format("Peer receiving: ~p~n", [Msg]),
                {Md5, PartsMd5, _Content} = find_in_list(Md5, File_Desc),
                From ! PartsMd5,
                State
            after 5000 ->
                State
        end,
    {Receive_peer_files, Receive_file_desc, LastAnnounce, LastPeerAsk} = NewState,
    %% Announcing
    case (erlang:system_time(second) - LastAnnounce) >= ?MIN_TRACKER_ANNOUNCE_INTERVAL of
        true ->
            io:format("Peer announcing~n"),
            AnnounceNewFiles = announce(Receive_peer_files, []),
            NewLastAnnounce = erlang:system_time(second),
            peer({AnnounceNewFiles, Receive_file_desc, NewLastAnnounce, LastPeerAsk});
        false ->
            io:format("Peer not announcing~n"),
            peer(NewState)
    end.

find_in_list(Find, [H|T]) ->
    {Md5, _PartsMd5, _Content} = H,
    case Md5 of
        Find ->
            H;
        _ ->
            find_in_list(Find, T)
    end.



call(Pid, Query) ->
    Tag = make_ref(),
    Pid ! {self(), Tag, Query},
    receive
        {Tag, Res} ->
            Res
    end.

cast(Pid, Query) ->
    Tag = make_ref(),
    Pid ! {self(), Tag, Query},
    Tag.