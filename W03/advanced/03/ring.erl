-module(ring).

-compile(export_all).

start(N, M) ->
    test.

loop(_NextPid, M, M) -> stop;
loop(NextPid, CountRec, M) ->
    receive
        X when is_integer(X) ->
            NextPid ! X+1,
            loop(NextPid, CountRec+1, M)
    end.