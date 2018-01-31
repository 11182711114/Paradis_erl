-module(multi_bank).
-compile(export_all).
-import(bank, [new/0,add/2,withdraw/2,balance/1]).

%%  This programs uses the module bank, by keeping a map of atom => bank. 
%%  The program is run by;
%%  test()                      --> unit tests
%%  new_manager()               --> spawn a new manager
%%  add(Pid, Who, X)            --> adds X amount to Who in the process Pid(manager)
%%                                      This is also how you add new atoms(people) to the bank_manager
%%  withdraw(Pid, Who, X)       --> same as above but withdraw X
%%  balance(Pid, Who)           --> returns the balance of the atom Who
%%  lend(Pid, Lender, To, X)    --> Lends X amount from Lender to To in the manager with Pid
%%                                      Lend works by withdrawing from the Lender and adding to To



%%  test()  --> Unit tests, checks all(valid) possible outcomes of the public functions.
%%      Running this should return the atom 'horray'
test() ->
    Pid = new_manager(),
    ok = add(Pid, joe, 20),
    20 = balance(Pid, joe),
    ok = withdraw(Pid, joe, 20),
    0 = balance(Pid, joe),
    insufficient_funds = withdraw(Pid, joe, 1),
    {incorrect_keys, [fred]} = lend(Pid, fred, joe, 20),
    ok = add(Pid, fred, 20),
    ok = lend(Pid, fred, joe, 20),
    ok = withdraw(Pid, joe, 20),
    {incorrect_keys, [sad, das]} = lend(Pid, sad, das, 20),
    {incorrect_key, socrates} = balance(Pid, socrates),
    {incorrect_keys, [sad]} = lend(Pid, sad, joe, 20),
    horray.

%%  new_manager() --> spawns a new process running the bank_manager() function
new_manager() ->
    spawn(fun() -> bank_manager(#{}) end).

%%  balance(Pid, Who) --> Returns the amount in the atoms(Who) bank
%%      Pid = Pid of the bank_manager spawned by new_manager/0
%%      Who = The atom representing the bank(account)
balance(Pid, Who) ->
    rpc(Pid, {balance, Who}).

%%  add(Pid, Who, X) --> Adds money to an existing atom => bank pair or creates a new pair,
%%      Pid = Pid of the bank_manager spawned by new_manager/0
%%      Who = The atom representing the bank(account)
%%      X   = The amount to add  
add(Pid, Who, X) ->
    rpc(Pid, {add, Who, X}).

%%  withdraw(Pid, Who, X) --> withdraws money from the bank represented by atom
%%      Pid = Pid of the bank_manager spawned by new_manager/0
%%      Who = The atom representing the bank(account)
%%      X   = The amount to withdraw 
withdraw(Pid, Who, X) ->
    rpc(Pid, {withdraw, Who, X}).

%%  lend(Pid, Lender, To, X) --> withdraws money from the bank represented by atom
%%      Pid     = Pid of the bank_manager spawned by new_manager/0
%%      Lender  = The atom representing the bank(account) of the lender
%%      To      = The atom representing the bank(account) of the recipient
%%      X       = The amount to withdraw
%%! Note: while add/3 allows for adding money to a nonexisting atom(by creating the pair) this method does not 
lend(Pid, From, To, Amount) ->
    rpc(Pid, {lend, From, To, Amount}).

%% Handles sending commands to a Pid, receiving a reply and returning the reply
rpc(Pid, X) ->
    Pid ! {self(), X},
    receive
        Any ->
            Any
    end.

    %% This function is used when multiple keys are checked
check_key_exists_in_map(Who, Map) when is_list(Who) ->
    %% Checks whether the keys in Who exists in the map Map
    %% Returns {correct_keys} when all keys are valid or
    %% Returns {incorrect_keys, X} where X is a list of invalid keys
    Incorrect_keys = [X || X <- Who, not maps:is_key(X,Map)],
    case Incorrect_keys of 
        [] ->
            {correct_keys};
        _Any ->
            {incorrect_keys, Incorrect_keys}
    end;
    %% This function is used when one key is checked
check_key_exists_in_map(Who, Map) ->
    %% Checks whether the key, Who, exists in the map Map
    %% Returns {correct_key, Who} when the key is valid
    %% Returns {incorrect_keys, Who} where Who is the invalid key
    IsKey = maps:is_key(Who,Map),
    if 
        not IsKey ->
            {incorrect_key, Who};
        true ->
            {correct_key, Who}
    end.

%% Handles withdraw for the bank_manager
%%? Whether this function should actually create a new atom, bank pair when the From atom is not in the map is debatable, 
%%? I would consider this logic bad.
manager_add(From, Who, Amount, Map) ->
    Op_bank = maps:get(Who, Map, bank:new()),   %% Get the value associated with Who or default to a new bank when Who is not a key in Map
    From ! bank:add(Op_bank, Amount),           %% Add the amount to the associated bank and send the result of operation to From
    Map#{Who => Op_bank}.                       %% Returns new map overriding prev bank of Who so we handle creating new banks without special logic

%% Handles withdraw for the bank_manager
%% Checks whether the key, Who, exists in Map,
%% If it doesnt sends a tuple, {incorrect_key, Who}, to From
%% If it does exist sends the value of bank:withdraw/2 of the associated value of Who to From
manager_withdraw(From, Who, Amount, Map) ->
    Key_exists = check_key_exists_in_map(Who, Map),
    case Key_exists of
        {incorrect_key, Who} -> 
            From ! Key_exists;

        {correct_key, Who} ->
            From ! bank:withdraw(maps:get(Who, Map), Amount)
    end,
    Map.

%% Handles lending functionality for bank_manager
%% Checks whether the keys, Lender, To, exist in Map
%% If they do bank:withdraw/2 is used to withdraw the lending amount(Amount) from Lender
%%  and uses bank:add/2 to adds it to To.
%% Lender is checked to have a sufficient balance by using bank:balance/2 and if it does not insufficient_funds is sent to From
%%! Note: while add/3 allows for adding money to a nonexisting atom(by creating the pair) this method does not
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

%%  Handles balance for bank_manager
%%  Simply checks if Who exists in Map and
%%  returns the result from bank:balance/2
%%  or {incorrect_key, Who} the key does not exist in Map.
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