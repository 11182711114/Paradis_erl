-module(peer).
-behaviour(gen_server).
-compile(export_all).


index_ip() -> "127.0.0.1".
index_port() -> 34243.
tracker_ip() -> "127.0.0.1".
tracker_port() -> 43534.

init(_Args) ->
    {ok, #{}}.

handle_call(Req, From, State) ->
    handle(From, Req, State).

handle_cast(Req, State) ->
    test.

handle(From, {i_am_interested_in, Md5}, State) ->
    test;
handle(From, {who_is_interested, Md5}, State) ->
    test;
handle(From, {what_have_you, Md5}, State) ->
    test;
handle(From, {send_me, Md5, Start, Stop}, State) ->
    test.

add_to_index(FileName, Md5, Size) ->
    gen_server:call(index, {add_to_index, FileName, Md5, Size}),
    gen_server:call(tracker, {i_have, Md5}).
