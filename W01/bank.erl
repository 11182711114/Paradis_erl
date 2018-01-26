-module(bank).
-compile(export_all).

test() ->
    Pid = new(),
    ok = add(Pid, 10),
    ok = add(Pid, 20),
    30 = balance(Pid),
    ok = withdraw(Pid, 15),
    15 = balance(Pid),
    insufficient_funds = withdraw(Pid, 20),
    horray.

new() ->
    spawn(fun() -> bank(0) end).

balance(Pid) ->
    rpc(Pid, {balance}).

add(Pid, X) -> 
    rpc(Pid, {add, X}).


withdraw(Pid, X) ->
    rpc(Pid, {withdraw, X}).

rpc(Pid, X) ->
    Pid ! {self(), X},
    receive
        Any ->
            Any
    end.

bank(X) ->
    receive
	{From, {add, Y}} ->
	    From ! ok,
	    bank(X+Y);
	{From, {withdraw, Y}} ->
        if 
            X-Y >= 0 ->
                From ! ok,
                bank(X-Y);
            true ->
                From ! insufficient_funds,
                bank(X)
        end;
	{From, {balance}} ->
        From ! X,
        bank(X)
    end.
