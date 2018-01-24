-module(multi_bank).
-compile(export_all).

test() ->
    Pid = new_manager(),
    ok = add(Pid, joe, 20),
    20 = balance(Pid, joe),
    ok = withdraw(Pid, joe, 20),
    0 = balance(Pid, joe),
    insufficient_funds = withdraw(Pid, joe, 1),
    {incorrect_key, fred} = lend(Pid, fred, joe, 20),
    ok = add(Pid, fred, 20),
    ok = lend(Pid, fred, joe, 20),
    ok = withdraw(Pid, joe, 20),
    {incorrect_keys, [sad, das]} = lend(Pid, sad, das, 20),
    {incorrect_key, socrates} = balance(Pid, socrates),
    horray.

new_manager() ->
    spawn(fun() -> bank_manager(#{}) end).

balance(Pid, Who) ->
    rpc(Pid, {balance, Who}).

add(Pid, Who, X) -> 
    rpc(Pid, {add, Who, X}).

withdraw(Pid, Who, X) ->
    rpc(Pid, {withdraw, Who, X}).

lend(Pid, From, To, Amount) ->
    rpc(Pid, {lend, From, To, Amount}).

rpc(Pid, X) ->
    Pid ! {self(), X},
    receive
        Any ->
            Any
    end.

bank_manager_check_key(Pid, Who, Map) when is_map(Map) ->
    IsKey = maps:is_key(Who,Map),
    if 
        not IsKey ->
            Pid ! {incorrect_key, Who},
            {incorrect_key, Who};
        true ->
            {correct_key, Who}
    end;
bank_manager_check_key(Who, Map, Fun) when is_function(Fun)  ->
    IsKey = maps:is_key(Who,Map),
    if 
        not IsKey ->
            Fun(Map)
    end.

bank_manager_check_key(Who, Map) when is_map(Map) ->
    IsKey = maps:is_key(Who,Map),
    if 
        not IsKey ->
            {incorrect_key, Who};
        true ->
            {correct_key, Who}
    end.

bank_manager(X) ->
    receive
        {From, {add, Who, Amount}} ->
            Op_bank = maps:get(Who, X, bank:new()), %
            From ! bank:add(Op_bank, Amount),
            bank_manager(X#{Who => Op_bank});       %% recur with new map overriding prev bank of Who

        {From, {withdraw, Who, Amount}} ->
            Key_exists = bank_manager_check_key(From, Who, X),
            if 
                Key_exists == {incorrect_key, Who} -> 
                    From ! Key_exists,
                    bank_manager(X) 
            end,
            From ! bank:withdraw(maps:get(Who, X), Amount),
            bank_manager(X);

        {From, {lend, Lender, To, Amount}} ->
            LenderExists = bank_manager_check_key(Lender, X),
            ToExists = bank_manager_check_key(To, X),
            if 
                LenderExists == {incorrect_key, Lender} andalso ToExists == {incorrect_key, To} ->
                    From ! {incorrect_keys, [Lender, To]};
                
                LenderExists == {incorrect_key, Lender} ->
                    From ! LenderExists;

                ToExists == {incorrect_key, To} ->
                    From ! ToExists;

                true ->
                    LenderProc = maps:get(Lender, X),
                    LenderBalance = bank:balance(LenderProc),
                    if 
                        LenderBalance >= Amount ->
                            bank:withdraw(LenderProc, Amount),
                            bank:add(maps:get(To, X), Amount),
                            From ! ok;
                  
                        true ->
                            From ! insufficient_funds
                    end
                end,
                bank_manager(X);

        {From, {balance, Who}} ->
            bank_manager_check_key(From, Who, X) == {incorrect_key, Who} andalso bank_manager(X),
            From ! bank:balance(maps:get(Who, X)),
            bank_manager(X)
    end.
