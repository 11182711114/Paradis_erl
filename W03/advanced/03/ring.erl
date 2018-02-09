-module(ring).

-compile(export_all).

time(N, M) ->
    {Time, Res} = timer:tc(?MODULE, start, [N,M]),
    {{time, Time}, {result, Res}}.

test() ->
    ok = test(50),
    hooray.

test(0) -> ok;
test(Num) ->
    N = rand:uniform(200),
    M = rand:uniform(200),
    Res = N*M,
    Res = start(N, M),
    test(Num-1).


start(N, M) ->
    {Pid, Processes} = create_ring(N, M, self()),
    Pid ! 0,
    Res = receive
        X -> X
    end,
    false = check_any_proc_alive(Processes),
    Res.

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

loop(_NextPid, M, M, FirstBol, RPid) -> % End of the loop
    if 
        FirstBol -> % if it is the first created we send the result back
            receive
                X when is_integer(X) ->
                    RPid ! X
            end;
        true ->     % otherwise we just stop
            stop
    end;
loop(NextPid, CountRec, M, FirstBol, RPid) ->
    receive
        Pid when is_pid(Pid) -> % The first process needs to know the last created process to complete the ring
            loop(Pid, CountRec, M, FirstBol, RPid);
        X when is_integer(X) ->
            NextPid ! X+1,
            loop(NextPid, CountRec+1, M, FirstBol, RPid)
    end.