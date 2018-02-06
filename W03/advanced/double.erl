-module(double).

-compile(export_all).


start() ->
    generic_server:start(?MODULE, fun doubler/1).


doubler(X) when is_integer(X) ->
    io:format("~p~n",[X*2]);
doubler(X) ->
    io:format("Non-integer input: ~p~n",[X]),
    throw(non_integer_input).