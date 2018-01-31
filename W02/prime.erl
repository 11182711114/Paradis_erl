-module(prime).
-compile(export_all).

% --- tests --- %

test() ->
    {test_is_prime, success} = test_is_prime(),
    {test_seq, success} = test_seq(),
    {test_filter, success} = test_filter(),
    {test_all_primes, success} = test_all_primes(),
    {test_safe_eval, success} = test_safe_eval(),
    hooray.

test_is_prime() ->
    true = is_prime(3),
    true = is_prime(7),
    false = is_prime(1),
    false = is_prime(10000),
    true = is_prime(21379),
    {test_is_prime, success}.

test_seq() ->
    [1,2,3,4,5,6,7,8,9,10] = seq(10),
    A = lists:seq(1,10000),
    A = seq(10000),
    B = lists:seq(1,13123),
    B = seq(13123),
    {test_seq, success}.

test_filter() ->
    %% sequential 
    [1,2,3,4] = filter(fun(X) -> X < 5 end, lists:seq(1,10)),
    [1,2,5,10] = filter(fun(X) -> 10 rem X == 0 end, lists:seq(1,20)),
    %% parallel 
    [1,2,3,4] = filterp(fun(X) -> X < 5 end, lists:seq(1,10)),
    [1,2,5,10] = filterp(fun(X) -> 10 rem X == 0 end, lists:seq(1,20)),
    {test_filter, success}.

test_all_primes() ->
    Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 
    67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 
    139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199],
    Primes = all_primes(200),
    Primes = all_primesp(200),
    {test_all_primes, success}.

test_safe_eval() ->
    {ok, 2} = safe_eval({plus,1,1}),
    {ok, 0} = safe_eval({minus,1,1}),
    {ok, 2} = safe_eval({minus,1,-1}),
    {ok, 3.0} = safe_eval({divide,21,7}),
    {ok, 26} = safe_eval({times,13,2}),
    {ok, 0} = safe_eval({plus,125,{minus,200,325}}),
    {ok, 2} = safe_eval({plus,1,1}), 
    {ok, 1.4} = safe_eval({divide,7,5}),
    {error, {divide_by_zero}} = safe_eval({plus,1,{times,4,{divide,4,0}}}),   
    {error, {divide_by_zero}} = safe_eval({plus,1,{divide,4,{minus,4,{plus,3,1}}}}),
    {error, {unknown_value, monday}} = safe_eval({plus,1,monday}),
    {error, {unknown_value, saturday}} = safe_eval({plus,1,{plus,saturday,4}}),
    {test_safe_eval, success}.

% --- benchmarks --- %
get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

bench_is_prime(N) ->
    A = get_timestamp(),
    is_prime_sieve(N),
    B = get_timestamp(),
    io:format("sieve/1: ~pms~n",[B-A]),
    C = get_timestamp(),
    all_primes(N),
    D = get_timestamp(),
    io:format("all_primes/1: ~pms~n",[D-C]).

bench_filters(N) ->
    F = fun(X) -> timer:sleep(100), X rem 63 == 0 end, 
    A = get_timestamp(),
    filter(F, lists:seq(1,N)),
    B = get_timestamp(),
    io:format("filter/2: ~pms~n",[B-A]),
    C = get_timestamp(),
    filterp(F, lists:seq(1,N)),
    D = get_timestamp(),
    io:format("filterp/2: ~pms~n",[D-C]).

bench_all_primes(N) ->
    A = get_timestamp(),
    all_primes(N),
    B = get_timestamp(),
    io:format("all_primes/1: ~pms~n",[B-A]),
    C = get_timestamp(),
    all_primesp(N),
    D = get_timestamp(),
    io:format("all_primesp/1: ~pms~n",[D-C]),
    E = get_timestamp(),
    sieve(N),
    F = get_timestamp(),
    io:format("sieve/1: ~pms~n",[F-E]).

% --- is_prime --- %
is_prime(1) -> false; %% assumption, 1 is not a prime
is_prime(N) -> is_prime(N, 2).

is_prime(_N, _N) -> true; %% If the recursion reaches the number then it is a prime
is_prime(N, Div) ->
    %% Checking if its a prime by starting from the bottom is more efficient than 
    %% starting from the top, e.g. is_prime(10000) req. 5000 iterations to determine 
    %% it is divisible by 5000 if we start from the top instead 
    %% of 1 iteration to find it is divisible by 2 if we start from the bottom.
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

sieve(N) ->
    prime(lists:seq(2,N)).

prime([]) -> [];
prime([Prime|Tail]) ->
    [Prime] ++ prime([N || N <- Tail, N rem Prime /= 0]).


% --- seq --- %
seq(N) when N > 0 -> seq(N,1).

seq(N, N) -> [N];
seq(N, Count) -> 
    [Count] ++ seq(N,Count+1).

% --- filter --- %
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


filterp(F, L) ->
    filterp_spawner(self(), F, L, 0),
    gather(0, length(L)).

filterp_spawner(_From, _F, [], _Num) -> [];
filterp_spawner(From, F, [H|T], Num) ->
    spawn(fun() -> filterp_body(From, F, H, Num) end),
    filterp_spawner(From, F, T, Num+1).

filterp_body(From, F, H, Num) ->
    case F(H) of
        true ->
            From ! {Num, [H]};
        false ->
            From ! {Num, []}
    end.

gather(End,End) -> [];
gather(X, End) ->
    receive
        {X, Res} ->
            Res ++ gather(X+1,End)
        after 25000 ->
            timeout
    end.

% --- all_primes --- %
all_primes(N) -> 
    filter(fun is_prime/1, seq(N)).

all_primesp(N) -> filterp(fun is_prime/1, seq(N)).

% --- rotate --- %
rotate(_N, []) -> [];
rotate(N, L) when N >= 0 ->
    rotate_body(N rem length(L), L); %% This should hold since for all groups of iterations length(L) the result will be the same as the original list
rotate(N, L) when N < 0 ->
    %% left rotation = right rotation on reversed list
    %% rotation iterations is the absolute number, e.g. -3 -> 3
    lists:reverse(rotate(abs(N), lists:reverse(L))).

rotate_body(0, L) -> L; %% Base case
rotate_body(N, [H|T]) ->
    rotate_body(N-1, T ++ [H]).


% --- Eval --- %
eval(E) ->
    case E of
        {plus,A,B} ->
            eval(A) + eval(B);
        {minus,A,B} ->
            eval(A) - eval(B);
        {times,A,B} ->
            eval(A) * eval(B);
        {divide,A,B} ->
            eval(A) / eval(B);
        X when is_integer(X) or is_float(X) ->
            X;
        Any ->
            {unknown_value,Any}
    end.

% --- safe_eval --- %
safe_eval(X) ->
    Evaluator = spawn(fun evaluator/0),
    rpc(Evaluator, X).
%%TODO: Fix tuple representation
%%TODO: e.g. {error,{divide,4,{plus,-2,2}}} -> {error,{divide,4,0}}
%%? Or not?
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
                A_val = eval_for_proc(A, From),
                B_val = eval_for_proc(B, From),
                if 
                    B_val == 0 -> throw(divide_by_zero);
                    true -> continue
                end,
                A_val / B_val;
            X when is_integer(X) or is_float(X) ->
                X;
            Any ->
                throw({unknown_value, Any})
        end
    catch
        error:badarith ->
            throw({error, From, {E}});
        throw:divide_by_zero ->
            throw({error, From, {divide_by_zero}});
        throw:{unknown_value, Val} ->
            throw({error, From, {unknown_value, Val}})
    end.

rpc(Pid, X) ->
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
        throw:{error, EFrom, Error} ->
            EFrom ! {error, Error}
    end,
    evaluator().
