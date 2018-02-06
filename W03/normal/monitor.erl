-module(monitor).

-compile(export_all).


start() ->
    spawn(?MODULE, mymonitor, [double, start]).


mymonitor(Mod, Func) ->
    Pid = apply(Mod,Func,[]),
    monitor(process, Pid),
    process_flag(trap_exit, true),
    receive
        {'EXIT', Pid, Why} ->
            io:format("~p crashed: ~p~n",[Pid,Why]),
            start();
        {'DOWN', _Tag, process, Proc, Why} ->
            io:format("~p crashed: ~p~n",[Proc, Why]),
            start();
        Any ->
            io:format("Unknown: ~p~n",[Any])
    end.