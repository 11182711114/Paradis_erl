-module(prime).
-compile(export_all).

is_prime(1) -> false; %% assumption, 1 is not a prime
is_prime(N) -> is_prime(N, 2).

is_prime(_N, _N) -> true; %% If the recursion reaches the number then it is a prime
is_prime(N, Div) ->
    %% Checking if its a prime by starting from the bottom is more efficient than 
    %% starting from the top, e.g. is_prime(10000) req. 5000 iterations to determine 
    %% it is divisible by 5000 by starting from the top instead of 1 iteration to find it is divisible by 2.
    %%! This is a slow algorithm, using a sieve would be more practical, see solution below.
    case (N rem Div) > 0 of
        true -> 
            is_prime(N, Div+1);
        false -> 
            false
    end.

is_prime_sieve(1) -> false;   
is_prime_sieve(N) -> 
    is_prime_sieve_body(lists:seq(2,N)).

is_prime_sieve_body([]) -> [];
is_prime_sieve_body([Prime|Tail]) -> 
    [Prime] ++ is_prime_sieve_body([N || N <- Tail, N rem Prime /= 0]).

get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

test(N) ->
    A = get_timestamp(),
    is_prime_sieve(N),
    B = get_timestamp(),
    io:format("sieve/1: ~pms~n",[B-A]),
    C = get_timestamp(),
    all_primes(N),
    D = get_timestamp(),
    io:format("all_primes/1: ~pms~n",[D-C]).


seq(N) -> seq(N,1).

seq(N, N) -> [N];
seq(N, Count) -> 
    [Count] ++ seq(N,Count+1).


filter_LC(F, L) -> %% Using list comprehension
    [X || X <- L, F(X)]. 

filter(_F, []) -> [];
filter(F, [H|T]) ->
    case F(H) of
        true ->
            [H] ++ filter(F,T);
        false ->
            filter(F,T)
    end.


all_primes(N) -> 
    filter(fun is_prime/1, seq(N)).


rotate(_N, []) -> [];
rotate(N, L) when N >= 0 -> %% O(N)
    rotate_body(N rem length(L), L); %% This should hold since for all groups of iterations length(L) the result will be the same as the original list
rotate(N, L) when N < 0 ->
    %% left rotation = right rotation on reversed list
    %% rotation iterations is the absolute number, e.g. -3 -> 3
    lists:reverse(rotate(abs(N), lists:reverse(L))).

rotate_body(0, L) -> L; %% Base case
rotate_body(N, [H|T]) ->
    rotate_body(N-1, T ++ [H]).



eval(E) ->
    case E of
        {plus,A,B} ->
            eval(A) + eval(B);
        {minus,A,B} ->
            eval(A) - eval(B);
        {times,A,B} ->
            eval(A) * eval(B);
        {divide,A,B} ->
            case eval_divide(A,B) of
                {error, divide_by_zero} -> error(divide_by_zero);
                X -> X
            end;
        X when is_integer(X) or is_float(X) ->
            X;
        Any ->
            {unknown_value,Any}
    end.

eval_divide(_A,0) ->
    {error, divide_by_zero};
eval_divide(A,B) ->
    eval(A) / eval(B).

eval_for_proc(E, From) ->
    try
        case E of
            {plus,A,B} ->
                eval_for_proc(A, From) + eval_for_proc(B, From);
            {minus,A,B} ->
                eval_for_proc(A, From) - eval_for_proc(B, From);
            {times,A,B} ->
                eval_for_proc(A, From) * eval_for_proc(B, From);
            {divide,A,B} ->
                eval_for_proc(A, From) / eval_for_proc(B, From);
            X when is_integer(X) or is_float(X) ->
                X;
            Any ->
                {unknown_value,Any}
        end
    catch
        error:badarith ->
            throw({error, From})
    end.


safe_eval(X) ->
    Evaluator = spawn(fun evaluator/0),
    eval_rpc(Evaluator, X).

eval_rpc(Pid, X) ->
    Pid ! {self(), X},
    receive
        Any -> Any
    end.

evaluator() ->
    try
        receive
            {From, {Op,A,B}} ->
                From ! {ok, eval_for_proc({Op, A,B}, From)};
            {From, X} when is_integer(X) or is_float(X) ->
                From ! {ok, X};
            {From, Any} ->
                From ! {unknown_value, Any};

            Any -> io:format("unknown message: ~p~n",[Any])
        end
    catch
        throw:{error, EFrom} ->
            EFrom ! {error, EFrom}
    end,
    evaluator().
