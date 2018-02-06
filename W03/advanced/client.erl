-module(client).

-compile(export_all).







loop(Tries) ->
    timer:sleep(rand:uniform(5000)),
    try double ! die of
        die -> loop(0)
    catch
        _:badarg ->
            true,
            loop(Tries+1);
        X ->
            io:format("~p~n",[X]),
            loop(Tries+1)
    end.