-module(list_op).
-compile(export_all).

% --- tests --- %
test() ->
    [3,4,5,1,2] = rotate(2, lists:seq(1,5)),
    [4,5,1,2,3] = rotate(-2, lists:seq(1,5)),
    [a,b,c,d,e] = rotate(5, [a,b,c,d,e]),
    [a,b,c,d,e] = rotate(-5, [a,b,c,d,e]),
    [c,d,e,a,b] = rotate(2, [a,b,c,d,e]),
    [1,2,3,4,5,6,7,8,9,10] = rotate(1000000000000000000000000000000000000000000000, lists:seq(1,10)),
    [2,3,4,5,6,7,8,9,10,1] = rotate(1243275328747812728391237182731627836172836178263170787543852603874816124325340572634718245131, lists:seq(1,10)),
    [10,1,2,3,4,5,6,7,8,9] = rotate(-1243275328747812728391237182731627836172836178263170787543812352603874816124325340572634718245131, lists:seq(1,10)),
    hooray.

% --- rotate --- %
rotate(_N, []) -> [];
rotate(N, L) when N >= 0 ->
    rotate_body(N rem length(L), L); %% all groups of iterations length(L) will result in the original list so we can remove them with rem
rotate(N, L) when N < 0 ->
    %% left rotation = right rotation on reversed list
    %% rotation iterations is the absolute number, e.g. -3 -> 3
    lists:reverse(rotate(abs(N), lists:reverse(L))).

rotate_body(0, L) -> L; %% Base case
rotate_body(N, [H|T]) ->
    rotate_body(N-1, T ++ [H]).

