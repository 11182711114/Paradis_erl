-module(hof).

-compile(export_all).

getTimeStamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%* This assumes that the length(L) to MaxWorkers ratio makes sense, i.e. >= 1.
pmap_timeout(F, L, Timeout, MaxWorkers) ->
    TimeoutTimeStamp = getTimeStamp()+Timeout,
    SplitList = list_op:splitList(L, MaxWorkers),
    spawn_worker(F, SplitList, TimeoutTimeStamp, self()).

gather(_TimeoutTimeStamp, Count, Count) -> [];
gather(TimeoutTimeStamp, Count, Final) ->
    TmpRem = TimeoutTimeStamp - getTimeStamp(),
    Timeleft = if TmpRem>=0 -> TmpRem; true -> 0 end,
    receive
        {Count, Res} ->
            [Res|gather(TimeoutTimeStamp, Count+1, Final)]
        after Timeleft ->
            timeout
    end.

spawn_worker(F, SplitList, Pid) ->
    spawn_worker(F, SplitList, Pid, 0).

spawn_worker(_F, [], _Pid, _Order) ->
    stop;
spawn_worker(F, [H|T], Pid, Order) ->
    spawn(?MODULE, pmap_timeout_worker, [F, H, Pid]),
    spawn_worker(F, T, Pid, Order+1).

pmap_timeout_worker(F, L, Num, Pid) ->
    Pid ! {Num, [X || X <- L, F(X)]}.


