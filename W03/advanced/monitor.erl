-module(monitor).

-compile(export_all).


start() ->
    spawn(?MODULE, mymonitor, [double, start]).


mymonitor(Mod, Func) ->
    % should be done using spawn_link()
    Pid = whereis(apply(Mod,Func,[])),  % Race condition
    process_flag(trap_exit, true),      
    link(Pid),                          % to this
    receive
        {'EXIT', Pid, _Why} ->
            io:format("~p crashed~n",[Pid]),
            start()
    end.