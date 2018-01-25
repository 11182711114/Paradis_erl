-module(prime).
-export([is_prime/1,seq/1,filter/2,filter_LC/2,all_primes/1,rotate/2]).

is_prime(1) ->
    false;
is_prime(N) ->
    is_prime(N, 2).

is_prime(_N, _N) ->
    true;
is_prime(N, D) ->
    NotDivisable = (N rem D) > 0,
    case NotDivisable of
        true -> 
            is_prime(N, D+1);
        false -> 
            false
    end.


seq(N) ->
    seq(N,1).

seq(N, N) ->
    [N];
seq(N, C) ->
    [C] ++ seq(N,C+1).

filter_LC(F, L) -> %% Using list comprehension
    [X || X <- L, F(X)].

filter(_F, []) ->
    [];
filter(F, [H|T]) ->
    case F(H) of
        true ->
            [H] ++ filter(F,T);
        false ->
            filter(F,T)
    end.

all_primes(N) ->
    filter(fun is_prime/1, seq(N)).

rotate(_N, []) ->
    [];
rotate(N, L) when N >= 0 ->
    rotate_body(N rem length(L), L); %% This should hold since for all groups of iterations length(L) the result will be the same as the original list
rotate(N, L) when N < 0 ->
    %% left rotation = right rotation on reversed list
    %% rotation iterations is the absolute number, e.g. -3 -> 3
    lists:reverse(rotate(abs(N), lists:reverse(L))).

rotate_body(0, L) -> L; %% Base case
rotate_body(N, [H|T]) when N > 0 ->
    rotate_body(N-1, T ++ [H]).
% rotate_body(N, L) when N < 0 -> %% Unnecessary
%     [H|T] = lists:reverse(L),
%     rotate_body(N+1, lists:reverse(T ++ [H])).