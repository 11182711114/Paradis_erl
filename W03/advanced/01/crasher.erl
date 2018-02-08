-module(crasher).

-compile(export_all).


start() ->
    spawn(?MODULE, crasher, []).

crasher() ->
    (catch link(client)), % For conveniance
    timer:sleep(rand:uniform(3)*1000),  % sleep for 1-3s
    (catch {double ! {self(), make_ref(), die}}),               % sending to registered name, will error if name is not registered, looking up the name with whereis(double) or similar is a race-condition
    crasher().