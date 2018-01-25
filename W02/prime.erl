-module(prime).
-compile(export_all).

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
rotate(N, L) ->
    rotate_body(N rem length(L), L). %% This should hold?

rotate_body(0, L) -> L;
rotate_body(N, [H|T]) ->
    rotate(N-1, T ++ [H]).