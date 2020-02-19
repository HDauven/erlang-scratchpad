%% @doc A collection of mathematical functions.
-module(maths).

-export([area/1, area/3, double/1, fac/1, factorial/1,
	 fib/1, fibNew/1, fibonacci/1, multiply/2, perfect/1,
	 square/1, termial/1]).

multiply(X, Y) -> X * Y.

double(X) -> multiply(2, X).

square(X) -> multiply(X, X).

area(A, B, C) ->
    S = (A + B + C) / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).

factorial(0) -> 1;
factorial(N) when N > 0 -> factorial(N - 1) * N.

% Tail recursion factorial:
fac(N) -> fac(N, 1).

% Second parameter is used as accumulator.
% fac/2 become 'private' functions.
fac(0, P) -> P;
fac(N, P) when N > 0 -> fac(N - 1, P * N).

% Additive analog of factorial.
% Sum of all positive ints equal to N.
termial(0) -> 0;
termial(N) when N > 0 -> termial(N - 1) + N.

fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) when N > 1 ->
    fibonacci(N - 1) + fibonacci(N - 2).

% Tail recursion fibonacci:
fib(0, P, _C) -> P;
fib(N, P, C) -> fib(N - 1, C, P + C).

fib(N) -> fib(N, 0, 1).

% Direct definition of adjecent pairs of fib nums
fibP(0) -> {0, 1};
fibP(N) -> {P, C} = fibP(N - 1), {C, P + C}.

fibNew(N) -> {P, _} = fibP(N), P.

perfect(N, N, S) -> N == S;
perfect(N, M, S) when N rem M == 0 ->
    perfect(N, M + 1, S + M);
perfect(N, M, S) -> perfect(N, M + 1, S).

perfect(N) -> perfect(N, 1, 0).

% Complex pattern matching on objects:
% {circle, {X,Y}, R}
% {rectangle, {X,Y}, H, W}
area({circle, {_X, _Y}, R}) -> math:pi() * R * R;
area({rectangle, {_X, _Y}, H, W}) -> H * W.

% Pattern matching allows to distinguish between
% cases and extract components.

