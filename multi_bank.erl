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

check_key_exists_in_map(Who, Map) when is_map(Map), is_list(Who) ->
    %% Checks weather the keys in Who exists in map Map
    %% Returns {correct_keys} when all keys are valid or
    %% Returns {incorrect_keys, X} where X is a list of invalid keys
    Incorrect_keys = [X || X <- Who, not maps:is_key(X,Map)],
    case Incorrect_keys of 
        [] ->
            {correct_keys};
        _Any ->
            {incorrect_keys, Incorrect_keys}
    end;
check_key_exists_in_map(Who, Map) when is_map(Map) ->
    IsKey = maps:is_key(Who,Map),
    if 
        not IsKey ->
            {incorrect_key, Who};
        true ->
            {correct_key, Who}
    end.


manager_add(From, Who, Amount, Map) ->
    Op_bank = maps:get(Who, Map, bank:new()), %% Get the value associated with Who or default to a new bank when Who is not a key in X
    From ! bank:add(Op_bank, Amount),       %% Add the amount to the associated bank and send the result of operation to From
    Map#{Who => Op_bank}.       %% Returnsnew map overriding prev bank of Who so we handle creating new banks without special logic

manager_withdraw(From, Who, Amount, Map) ->
    Key_exists = check_key_exists_in_map(Who, Map),
    case Key_exists of
        {incorrect_key, Who} -> 
            From ! Key_exists;

        {correct_key, Who} ->
            From ! bank:withdraw(maps:get(Who, Map), Amount)
    end,
    Map.

manager_lend(From, Lender, To, Amount, Map) ->
    Incorrect_keys = check_key_exists_in_map([Lender, To], Map),
    case Incorrect_keys of 
        {incorrect_keys, _Any} ->
            From ! Incorrect_keys;

        {correct_keys} ->
            LenderProc = maps:get(Lender, Map),
            LenderBalance = bank:balance(LenderProc),
            if 
                LenderBalance >= Amount ->
                    bank:withdraw(LenderProc, Amount),
                    bank:add(maps:get(To, Map), Amount),
                    From ! ok;
            
                true ->
                    From ! insufficient_funds
            end
    end,
    Map.

manager_balance(From, Who, Map) ->
    Bank = maps:get(Who, Map, {incorrect_key, Who}),
    case Bank of 
        {incorrect_key, Who} ->
            From ! Bank;

        Existing_bank ->
            From ! bank:balance(Existing_bank)
    end,
    Map.

% @doc Keeps track of banks with a map(atom => bank) and acts on them
% @author Fredrik Larsson
% @version 1.0
% @param X map of atom => bank
bank_manager(X) ->
    receive
        {From, {add, Who, Amount}} ->            
            bank_manager(manager_add(From, Who, Amount, X));

        {From, {withdraw, Who, Amount}} ->
            bank_manager(manager_withdraw(From, Who, Amount, X));

        {From, {lend, Lender, To, Amount}} ->
            bank_manager(manager_lend(From, Lender, To, Amount, X));
            
        {From, {balance, Who}} ->
            bank_manager(manager_balance(From, Who, X))
    end.