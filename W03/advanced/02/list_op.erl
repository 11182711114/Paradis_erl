-module(list_op).

-compile(export_all).

test(Timeout, Workers) ->
    {Time, _Res} = timer:tc(?MODULE, pmap_maxtime, [fun misc:fib/1, [rand:uniform(250) || _ <- lists:seq(1,200)], Timeout, Workers]),
    true = (Time/1000)-50 =< Timeout, % this is soft real-time after all
    Time/1000.


% Entry functions
pmap_timeout(F, L, Timeout, MaxWorkers) ->
    pmap(F, L, Timeout, MaxWorkers, timeout).
pmap_maxtime(F, L, Timeout, MaxWorkers) ->
    pmap(F, L, Timeout, MaxWorkers, deadline).

% The high level work
pmap(F, L, Timeout, MaxWorkers, TimeoutStrategy) ->
    NewTimeout = if TimeoutStrategy == deadline -> misc:getTimeStamp()+Timeout; true -> Timeout end, % if its a deadline(maxtime) we need the deadline time
%   Split the list into parts that the different workers will use. 
%       This is the only way the number of workers is limited
    SplitList = split_list_n_parts(L, MaxWorkers),
    all_ok = spawn_supervisors(F, SplitList, self(), NewTimeout, TimeoutStrategy), % Spawns supervisors that will spawn workers NewTimeout do the work
    Res = gather(0, length(SplitList), []), % Gathers the results recursivly in order
    lists:reverse(lists:flatten(Res)).


%% When Count = Final -> return State
gather(Final, Final, State) -> State;
%% Recursivly gather the parts in order
gather(Count, Final, State) ->
    receive
        {Count, Res} ->
            gather(Count+1, Final, [Res|State])
    end.

%% Conveniance entry functions
spawn_supervisors(F, SplitList, Pid, Deadline, TimeoutStrategy) ->
    spawn_supervisors(F, SplitList, Pid, 0, Deadline, TimeoutStrategy).

%% Spawns a worker for every list in the deeplist
spawn_supervisors(_F, [], _Pid, _Order, _Deadline, _TimeoutStrategy) -> all_ok; %% Base case
spawn_supervisors(F, [H|T], Pid, Order, Deadline, TimeoutStrategy) ->
    spawn(?MODULE, worker_supervisor, [F, H, Order, Pid, [], Deadline, TimeoutStrategy]),
    spawn_supervisors(F, T, Pid, Order+1, Deadline, TimeoutStrategy).

%% A worker supervisor, will spawn a worker for each element in the given list and wait for the specified time for a message with the result.
%% Note that while this spawns concurrent processes the work is done sequencially
worker_supervisor(_F, [], Num, Pid, Res, _Deadline, _TimeoutStrategy) -> Pid ! {Num, Res};
worker_supervisor(F, [H|T], Num, Pid, Res, Deadline, TimeoutStrategy) ->
    Tag = make_ref(),
    Worker = spawn(?MODULE, worker, [F, H, {self(), Tag}]),
    TmpRem = case TimeoutStrategy of
        deadline -> Deadline - misc:getTimeStamp();     % Convert deadline NewTimeout time left for use in after
        timeout -> Deadline
    end,
    Timeleft = if TmpRem>=0 -> TmpRem; true -> 0 end,   % Make sure Timeleft>=0
    FRes = receive
        {Tag, X} -> X
        after Timeleft -> exit(Worker, timeout), timeout    % Kill and return timeout if we are out of time
    end,
    NewRes = [FRes|Res],
    worker_supervisor(F, T, Num, Pid, NewRes, Deadline, TimeoutStrategy).

% Actual worker
worker(F, Item, {Pid, Tag}) ->
    Res = try F(Item)
    catch error:_ -> error
    end,
    Pid ! {Tag, Res}.

    
% splits a list L into NrParts, 
% The L rem NrParts is added to the first list in the sublists,
%   e.g. L = 10, NrParts = 4 -> 4,2,2,2
split_list_n_parts(L, NrParts) when NrParts < length(L) ->
    Length = length(L),
    Rem = Length rem NrParts,
    FirstLen = trunc(((Length-Rem) / NrParts) + Rem),   % Length of the first list, trunc for float -> int conversion
    ParLen = trunc((Length-FirstLen) / (NrParts-1)),    % Length of the rest of the lists
    {FirstList, Rest} = lists:split(FirstLen, L),
    [FirstList | split_list_n_parts_recur(Rest, ParLen)].

split_list_n_parts_recur([], _ParLen) -> [];
split_list_n_parts_recur(L, ParLen) ->
    {Sub, Rest} = lists:split(ParLen, L),
    [Sub | split_list_n_parts_recur(Rest, ParLen)].
