-module(multi_bank).
-compile(export_all).
-import(bank, [new/0,add/2,withdraw/2,balance/1]).

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

bank_manager_check_key(Who, Map) when is_map(Map), is_list(Who) ->
    Incorrect_keys = [X || X <- Who, not maps:is_key(X,Map)],
    if 
        length(Incorrect_keys) > 0 ->
            {incorrect_keys, Incorrect_keys};
        true ->
            {correct_keys}
    end;
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
            bank_manager(X#{Who => Op_bank});       %% recur with new map overriding prev bank of Who so we handle creating new banks without special logic

        {From, {withdraw, Who, Amount}} ->
            Key_exists = bank_manager_check_key(Who, X),
            case Key_exists of
                {incorrect_key, Who} -> 
                    From ! Key_exists,
                    bank_manager(X);

                {correct_key, Who} ->
                    From ! bank:withdraw(maps:get(Who, X), Amount),
                    bank_manager(X)
            end;

        {From, {lend, Lender, To, Amount}} ->
            Incorrect_keys = bank_manager_check_key([Lender, To], X),
            case Incorrect_keys of 
                {incorrect_keys, _Any} ->
                    From ! Incorrect_keys;

                {correct_keys} ->
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
            Bank = maps:get(Who, X, {incorrect_key, Who}),
            case Bank of 
                {incorrect_key, Who} ->
                    From ! Bank;

                Existing_bank ->
                    From ! bank:balance(Existing_bank)
            end,
            bank_manager(X)
    end.