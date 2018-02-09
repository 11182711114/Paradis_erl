-module(ring).

-compile(export_all).

time(N, M) ->
    {Time, Res} = timer:tc(?MODULE, start, [N,M]),
    {{time, Time}, {result, Res}}.

start(N, M) ->
    create_ring(N, M, self()) ! 0,
    receive
        X -> X
    end.

create_ring(N, M, RPid) ->
    Pid = spawn(?MODULE, loop, [null, 0, M, true, RPid]),
    Pid ! create_ring_body(Pid, M, 1, N),
    Pid.

create_ring_body(PrevPid, _M, N, N) -> PrevPid;
create_ring_body(PrevPid, M, CurrentN, N) ->
    Pid = spawn(?MODULE, loop, [PrevPid, 0, M, false, null]),
    create_ring_body(Pid, M, CurrentN+1, N).

loop(_NextPid, M, M, FirstBol, RPid) -> % End of the loop
    if 
        FirstBol -> % if it is the first created we send the result back
            receive
                X when is_integer(X) ->
                    RPid ! X
            end;
        true -> % otherwise we just stop
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