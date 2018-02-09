-module(monitor).

-compile(export_all).


start() ->
    spawn(?MODULE, mymonitor, [double, start]).

% Mod, Func are to a function that spawns and registers a server and returns the registered name.
% This should be done by passing the main loop instead and spawn_link() it here so that the loop
%   is not allowed to potentially crash before we link to it.
mymonitor(Mod, Func) ->
    % should be done using spawn_link()
    Pid = whereis(apply(Mod,Func, [])),  % Race condition
    process_flag(trap_exit, true),      
    link(Pid),                          % to this
    receive
        {'EXIT', Pid, _Why} ->
            io:format("~p crashed~n",[Pid]),
            start()
    end.