-module(generic_server).

-compile(export_all).


start(Mod, Func) ->
    register(Mod, spawn(generic_server, generic_server, [Mod, Func])),
    Mod.

generic_server(Mod, Func) ->
    receive
        {From, Tag, Query} ->       
            Reply = Func(Query),
            From ! {Tag, Reply},
            generic_server(Mod, Func)
    end.


call(Pid, Q) ->
    Tag = make_ref(),
    Pid ! {self(), Tag, Q},
    receive
        {Tag, Res} ->
            Res
    end.