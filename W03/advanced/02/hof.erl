-module(hof).

-compile(export_all).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

getTimeStamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%* This assumes that the length(L) to MaxWorkers ratio makes sense, i.e. >= 1.
pmap_maxtime(F, L, Timeout, MaxWorkers) ->
    Deadline = getTimeStamp()+Timeout, % Convert a timeout counter to a deadline
%   Split the list into parts that the different workers will use, 
%       this is how the number of workers is limited
    SplitList = list_op:splitList(L, MaxWorkers), 
    all_ok = spawn_worker(F, SplitList, self(), Deadline), % Spawn workers for the lists
    Res = gather(Deadline, 0, length(SplitList), []),
    lists:reverse(lists:flatten(Res)).

%% When Count = Final -> return State
gather(_TimeoutTimeStamp, Count, Count, State) -> State;
%% Recursivly gather the parts in order, if we time out and result is not in the mailbox
%% we will fail and add timeout instead
gather(Deadline, Count, Final, State) ->
    receive
        {Count, Res} ->
            gather(Deadline, Count+1, Final, [Res|State])
    end.

%% Conveniance entry function
spawn_worker(F, SplitList, Pid, Deadline) ->
    spawn_worker(F, SplitList, Pid, 0, Deadline).

%% Spawns a worker for every list in the deeplist
spawn_worker(_F, [], _Pid, _Order, _Deadline) -> all_ok; %% Base case
spawn_worker(F, [H|T], Pid, Order, Deadline) ->
    spawn(?MODULE, pmap_timeout_worker, [F, H, Order, Pid, [], Deadline]),
    spawn_worker(F, T, Pid, Order+1, Deadline).

%% A worker with list comprehension
pmap_timeout_worker_LC(F, L, Num, Pid) ->
    Pid ! {Num, [F(X) || X <- L]}.

%% A worker, runs F(X) on head recursivly through the list until nothing is left and then sends back Res
pmap_timeout_worker(_F, [], Num, Pid, Res, _Deadline)   -> Pid ! {Num, Res};
pmap_timeout_worker(F, [H|T], Num, Pid, Res, Deadline) ->
%TODO: Do the F(H) with a spawned worker so we can return timeout for the individual items in list
    spawn(?MODULE, pmap_actual_worker, [F, H, self()]),
    TmpRem = Deadline - getTimeStamp(), % Convert deadline to time left for use in after
    Timeleft = if TmpRem>=0 -> TmpRem; true -> 0 end, % Make sure timeleft is >=0
    FRes = receive
        X -> X

        after Timeleft ->
            timeout
    end,
    NewRes = [FRes|Res],
    pmap_timeout_worker(F, T, Num, Pid, NewRes, Deadline).

pmap_actual_worker(F, Item, Pid) ->
    Res = try F(Item)
    catch error:_ -> error
    end,
    Pid ! Res.