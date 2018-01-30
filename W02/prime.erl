-module(prime).
-compile(export_all).

is_prime(1) -> false; %% assumption, 1 is not a prime
is_prime(N) -> is_prime(N, 2).

is_prime(_N, _N) -> true; %% If the recursion reaches the number then it is a prime
is_prime(N, Div) ->
    %% Checking if its a prime by starting from the bottom is more efficient than 
    %% starting from the top, e.g. is_prime(10000) req. 5000 iterations to determine 
    %% it is divisible by 5000 by starting from the top instead of 1 iteration to find it is divisible by 2. 
    case (N rem Div) > 0 of
        true -> 
            is_prime(N, Div+1);
        false -> 
            false
    end.

get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

test(N) ->
    A = get_timestamp(),
    sieve(N),
    B = get_timestamp(),
    io:format("sieve/1: ~pms~n",[B-A]),
    C = get_timestamp(),
    all_primes(N),
    D = get_timestamp(),
    io:format("all_primes/1: ~pms~n",[D-C]).


sieve(N) ->
    prime(lists:seq(2,N)).

prime([]) -> [];
prime([Prime|Tail]) ->
    [Prime] ++ prime([N || N <- Tail, N rem Prime /= 0]).



seq(N) when N > 0 -> seq(N,1).

seq(N, N) -> [N];
seq(N, Count) -> [Count] ++ seq(N,Count+1).


filter_LC(F, L) -> %% Using list comprehension
    [X || X <- L, F(X)].


%% Make this parallel
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
    end.

test_filters(N) ->
    F = fun(X) -> timer:sleep(100), X rem 63 == 0 end, 
    A = get_timestamp(),
    filter(F, lists:seq(1,N)),
    B = get_timestamp(),
    io:format("filter/2: ~pms~n",[B-A]),
    C = get_timestamp(),
    filterp(F, lists:seq(1,N)),
    D = get_timestamp(),
    io:format("filterp/2: ~pms~n",[D-C]).



all_primes(N) -> filter(fun is_prime/1, seq(N)).

all_primesp(N) -> filterp(fun is_prime/1, seq(N)).

test_primes(N) ->
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
% rotate_body(N, L) when N < 0 -> %% Unnecessary
%     [H|T] = lists:reverse(L),
%     rotate_body(N+1, lists:reverse(T ++ [H])).



% eval(E) ->
%     case E of
%         {plus,A,B} ->
%             eval(A) + eval(B);
%         {minus,A,B} ->
%             eval(A) - eval(B);
%         {times,A,B} ->
%             eval(A) * eval(B);
%         {divide,A,B} ->
%             case eval_divide(A,B) of
%                 {error, divide_by_zero} ->
%                     test;
%                 X ->
%                     X
%             end;
%         X when is_integer(X) or is_float(X) ->
%             X;
%         Any ->
%             {unknown_value,Any}
%     end.

% eval_divide(_A,0) ->
%     {error, divide_by_zero};
% eval_divide(A,B) ->
%     eval(A) / eval(B).

% eval_rpc(Pid, X) ->
%     Pid ! {self(), X},
%     receive
%         Any -> Any
%     end.

% evaluator() ->
%     receive
%         {plus,A,B} ->
%             eval(A) + eval(B);
%         {minus,A,B} ->
%             eval(A) - eval(B);
%         {times,A,B} ->
%             eval(A) * eval(B);
%         {divide,A,B} ->
%             case eval_divide(A,B) of
%                 {error, divide_by_zero} ->
%                     test;
%                 X ->
%                     X
%             end;
%         X when is_integer(X) or is_float(X) ->
%             X;
%         Any ->
%             {unknown_value,Any}
%     end.
