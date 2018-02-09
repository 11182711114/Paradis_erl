-module(ring).

-compile(export_all).

% --- tests --- %
% Tests I iterations of start using rand:uniform(N), rand:uniform(M) as arguments.
test(I, N, M) ->
    ok = test_rand(I, N, M),
    hooray.

test_rand(0, _N, _M) -> ok;
test_rand(Num, InN, InM) ->
    N = rand:uniform(InN),
    M = rand:uniform(InM),
    Res = N*M,
    Res = start(N, M),
    test_rand(Num-1, InN, InM).


% --- public funcs --- % 
time(N, M) ->
    {Time, Res} = timer:tc(?MODULE, start, [N,M]),
    {{time, Time}, {result, Res}}.

start(N, M) ->
    {Pid, Processes} = create_ring(N, M, self()),
    Pid ! 0,
    Res = receive
        X -> X
    end,
    false = check_any_proc_alive(Processes),
    Res.

% --- internal funcs --- %
check_any_proc_alive([]) -> false;
check_any_proc_alive([H|T]) ->
    case process_info(H) of
        undefined -> check_any_proc_alive(T);
        _ -> true
    end.

create_ring(N, M, RPid) ->
    Pid = spawn(?MODULE, loop, [null, 0, M, true, RPid]),
    {PrevPid, Processes} = create_ring_body(Pid, M, 1, N, [Pid]),
    Pid ! PrevPid,
    {Pid, Processes}.

create_ring_body(PrevPid, _M, N, N, Processes) -> {PrevPid, Processes};
create_ring_body(PrevPid, M, CurrentN, N, Processes) ->
    Pid = spawn(?MODULE, loop, [PrevPid, 0, M, false, null]),
    create_ring_body(Pid, M, CurrentN+1, N, [Pid|Processes]).

% --- processes loop --- %
loop(_NextPid, M, M, FirstBol, RPid) -> % Final iteration of the server
    if 
        FirstBol ->     % if it is the first created we send the result back
            receive
                X -> RPid ! X
            end;
        true -> stop    % otherwise we just stop
            
    end;
loop(NextPid, CountRec, M, FirstBol, RPid) ->
    receive
        Pid when is_pid(Pid) -> % The first process needs to be sent the last created process to complete the ring
            loop(Pid, CountRec, M, FirstBol, RPid);
        X when is_integer(X) -> % Main functionality
            NextPid ! X+1,
            loop(NextPid, CountRec+1, M, FirstBol, RPid)
    end.