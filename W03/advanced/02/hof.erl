-module(hof).

-compile(export_all).

getTimeStamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%* This assumes that the specified length(L) to MaxWorkers ratio makes sense, i.e. >= 1.
pmap_timeout(F, L, Timeout, MaxWorkers) ->
    TimeoutAbs = getTimeStamp()+Timeout,
    SplitList = list_op:splitList(L, MaxWorkers),
    spawn_worker(F, SplitList, TimeoutAbs, self()).

gather(_, Count, Count) -> [];
gather(TimeoutAbs, Count, Final) ->
    Timeleft = TimeoutAbs - getTimeStamp(),
    receive
        {Count, Res} ->
            [Res|gather(TimeoutAbs, Count+1, Final)]
        after Timeleft ->
            timeout
    end.

spawn_worker(F, SplitList, Timeout, Pid) ->
    spawn_worker(F, SplitList, Timeout, Pid, 0).

spawn_worker(_F, [], _Timeout, _Pid, _Order) ->
    stop;
spawn_worker(F, [H|T], Timeout, Pid, Order) ->
    spawn(?MODULE, pmap_timeout_worker, [F, H, Pid]),
    spawn_worker(F, T, Timeout, Pid, Order+1).

pmap_timeout_worker(F, L, Num, Pid) ->
    Pid ! {Num, [X || X <- L, F(X)]}.


