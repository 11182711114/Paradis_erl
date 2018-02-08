-module(hof).

-compile(export_all).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).

getTimeStamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

%* This assumes that the length(L) to MaxWorkers ratio makes sense, i.e. >= 1.
pmap_timeout(F, L, Timeout, MaxWorkers) ->
    TimeoutTimeStamp = getTimeStamp()+Timeout, % Convert a timeout counter to a deadline
%   Split the list into parts that the different workers will use, 
%       this is how the number of workers is limited
    SplitList = list_op:splitList(L, MaxWorkers), 
    all_ok = spawn_worker(F, SplitList, self()), % Spawn workers for the lists
    Res = gather(TimeoutTimeStamp, 0, length(SplitList), []),
    lists:reverse(lists:flatten(Res)).

%% When Count = Final -> return State
gather(_TimeoutTimeStamp, Count, Count, State) -> State;
%% Recursivly gather the parts in order, if we time out and result is not in the mailbox
%% we will fail and add timeout instead
gather(TimeoutTimeStamp, Count, Final, State) ->
    TmpRem = TimeoutTimeStamp - getTimeStamp(), % Convert deadline to time left for use in after
    Timeleft = if TmpRem>=0 -> TmpRem; true -> 0 end, % Make sure timeleft is >=0
    receive
        {Count, Res} ->
            gather(TimeoutTimeStamp, Count+1, Final, [Res|State])
        after Timeleft ->
            gather(TimeoutTimeStamp, Count+1, Final, [timeout|State])
    end.

%% Conveniance entry function
spawn_worker(F, SplitList, Pid) ->
    spawn_worker(F, SplitList, Pid, 0).

%% Spawns a worker for every list in the deeplist
spawn_worker(_F, [], _Pid, _Order) -> all_ok; %% Base case
spawn_worker(F, [H|T], Pid, Order) ->
    spawn(?MODULE, pmap_timeout_worker, [F, H, Order, Pid, []]),
    spawn_worker(F, T, Pid, Order+1).

%% A worker with list comprehension
pmap_timeout_worker_LC(F, L, Num, Pid) ->
    Pid ! {Num, [F(X) || X <- L]}.

%% A worker, runs F(X) on head recursivly through the list until nothing is left and then sends back Res
pmap_timeout_worker(_F, [], Num, Pid, Res)   -> Pid ! {Num, Res};
pmap_timeout_worker(F, [H|T], Num, Pid, Res) ->
%TODO: Do the F(H) with a spawned worker so we can return timeout for the individual items in list
    Worker = spawn(?MODULE, pmap_actual_worker, [F, H, self()]),
    FRes = receive
        X -> X

        % after Timeleft ->
        %     timeout
    end,
    NewRes = [FRes|Res],
    pmap_timeout_worker(F, T, Num, Pid, NewRes).

pmap_actual_worker(F, Item, Pid) ->
    Res = try F(Item)
    catch error:_ -> error
    end,
    Pid ! Res.