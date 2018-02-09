-module(double).

-compile(export_all).

%%% Sequencial double server stuff
start() ->
    generic_server:start(?MODULE, fun doubler/1).


doubler(X) when is_integer(X) ->
    X*2;
doubler(X) ->
    io:format("Non-integer input: ~p~n",[X]),
    throw(non_integer_input).