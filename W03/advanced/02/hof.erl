-module(hof).

-compile(export_all).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

getTimeStamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).


pmap_timeout(F, L, Timeout, MaxWorkers) ->
    pmap(F, L, Timeout, MaxWorkers, timeout).
pmap_maxtime(F, L, Timeout, MaxWorkers) ->
    pmap(F, L, Timeout, MaxWorkers, maxtime).

pmap(F, L, Timeout, MaxWorkers, Type) ->
    TO = if Type == maxtime -> getTimeStamp()+Timeout; true -> Timeout end,
%   Split the list into parts that the different workers will use, 
%       this is how the number of workers is limited
    SplitList = list_op:splitList(L, MaxWorkers),
    all_ok = spawn_worker(F, SplitList, self(), TO, Type),
    Res = gather(0, length(SplitList), []),
    lists:reverse(lists:flatten(Res)).


%% When Count = Final -> return State
gather(Final, Final, State) -> State;
%% Recursivly gather the parts in order, if we time out and result is not in the mailbox
%% we will fail and add timeout instead
gather(Count, Final, State) ->
    receive
        {Count, Res} ->
            gather(Count+1, Final, [Res|State])
    end.

%% Conveniance entry function
spawn_worker(F, SplitList, Pid, Deadline, maxtime) ->
    spawn_worker_maxtime(F, SplitList, Pid, 0, Deadline);
spawn_worker(F, SplitList, Pid, Timeout, timeout) ->
    spawn_worker_timeout(F, SplitList, Pid, 0, Timeout).

%% Spawns a worker for every list in the deeplist
spawn_worker_maxtime(_F, [], _Pid, _Order, _Deadline) -> all_ok; %% Base case
spawn_worker_maxtime(F, [H|T], Pid, Order, Deadline) ->
    spawn(?MODULE, pmap_maxtime_worker, [F, H, Order, Pid, [], Deadline]),
    spawn_worker_maxtime(F, T, Pid, Order+1, Deadline).

spawn_worker_timeout(_F, [], _Pid, _Order, _Timeout) -> all_ok; %% Base case
spawn_worker_timeout(F, [H|T], Pid, Order, Timeout) ->
    spawn(?MODULE, pmap_timeout_worker, [F, H, Order, Pid, [], Timeout]),
    spawn_worker_timeout(F, T, Pid, Order+1, Timeout).

%% A worker, runs F(X) on head recursivly through the list until nothing is left and then sends back Res
pmap_maxtime_worker(_F, [], Num, Pid, Res, _Deadline)   -> Pid ! {Num, Res};
pmap_maxtime_worker(F, [H|T], Num, Pid, Res, Deadline) ->
    spawn(?MODULE, pmap_actual_worker, [F, H, self()]),
    TmpRem = Deadline - getTimeStamp(), % Convert deadline to time left for use in after
    Timeleft = if TmpRem>=0 -> TmpRem; true -> 0 end, % Make sure timeleft is >=0
    FRes = receive
        X -> X
        after Timeleft -> timeout
    end,
    NewRes = [FRes|Res],
    pmap_maxtime_worker(F, T, Num, Pid, NewRes, Deadline).

pmap_timeout_worker(_F, [], Num, Pid, Res, _Timeout)   -> Pid ! {Num, Res};
pmap_timeout_worker(F, [H|T], Num, Pid, Res, Timeout) ->
    spawn(?MODULE, pmap_actual_worker, [F, H, self()]),
    FRes = receive
        X -> X
        after Timeout -> timeout
    end,
    NewRes = [FRes|Res],
    pmap_timeout_worker(F, T, Num, Pid, NewRes, Timeout).

% actual worker
pmap_actual_worker(F, Item, Pid) ->
    Res = try F(Item)
    catch error:_ -> error
    end,
    Pid ! Res.