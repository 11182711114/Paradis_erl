-module(bad_prog1).

%
%   This program has a number of errors
%    find them and fix them.
%    Once you have fixed the code
%    run some text examples to see if the fuctions
%    behave as expected
%    Running bad_prog1:test() should return the atom 'hooray'
% 

-compile(export_all).

test_all() ->
    10 = double(5),
    100 = area({square,10}),
    40 = perimeter({square,10}),
    % melting point of sulfur 
    {f,212.0} = temperature_convert({c,100}), 
    24 = factorial(4),
    hooray.

factorial(0) -> 1;
factorial(N) -> N*factorial(N-1).

test1() ->
    io:format("double(2) is ~p~n",[double(2)]).

double(X) ->
    2*X.

area({square,X}) ->
    X*X;
area({rectangle,X,Y}) ->
    X*Y;
area({circle,R}) ->
    math:pi()*math:pow(R,2).

% temperature conversion 
% using formula 5(F-32) = 9C

temperature_convert({c,C}) -> 
    F = C*9/5+32,
    {f,F};
temperature_convert({f,F}) -> 
    C = (F-32)*5/9,
    {c,C}.

perimeter(X) ->
    case X of
        {rectangle,Z,Y} ->
             2*(Z+Y);
	    {square,Z} ->
             4*Z;
        _ ->
            io:format("cannot compute the area of ~p~n",[X])
    end.


