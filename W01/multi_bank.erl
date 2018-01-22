-module(multi_bank).

-compile(export_all).

-import(bank,[new/0, balance/1, add/2, withdraw/2]).

test() ->
    Pid = new_manager(),
    ok = add(Pid, joe, 20),
    ok = add(Pid, fred, 40),
    ok = lend(Pid, joe, fred, 20),
    0 = balance(Pid, joe),
    60 = balance(Pid, fred),
    ok = withdraw(Pid, fred, 50),
    insufficient_funds = withdraw(Pid, joe, 1),
    ok = lend(Pid, fred, joe, 10),
    ok = withdraw(Pid, joe, 10),
    0 = balance(Pid, fred),
    0 = balance(Pid, joe),
    ok = lend(Pid, joe, fred, 0),
    0 = balance(Pid, fred),
    0 = balance(Pid, joe),
    insufficient_funds = lend(Pid, joe, fred, 1),
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
    BalanceFrom = balance(Pid, From),
    if 
        BalanceFrom >= Amount ->
            ok = withdraw(Pid, From, Amount),
            ok = add(Pid, To, Amount),
            ok;
        true -> %% Not enough funds in From
            insufficient_funds
    end.


rpc(Pid, X) ->
    Pid ! {self(), X},
    receive
        Any -> Any
    end.

bank_manager(X) ->
    receive
        {From, {balance, Name}} ->
            From ! bank:balance(maps:get(Name, X));

        {From, {add, Name, Amount}} ->
            KeyExists = maps:is_key(Name, X),
            if 
                 KeyExists ->
                    From ! bank:add(maps:get(Name, X), Amount);
                true ->
                    NewAcc = bank:new(),
                    From ! bank:add(NewAcc, Amount),
                    bank_manager(maps:put(Name, NewAcc, X))
            end;

        {From, {withdraw, Name, Amount}} ->
            From ! bank:withdraw(maps:get(Name, X), Amount)
    end,
    bank_manager(X). %% Default, no change