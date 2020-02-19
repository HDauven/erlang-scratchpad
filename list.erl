%% @doc A collection of functions that operate on lists.
-module(list).

-export([bun/1, maximum/1, multiply/1, nub/1,
	 palindrome/1, sum/1, sumOptimized/1, take/2]).

% Direct definition of sum
sum([]) -> 0;
sum([X | Xs]) -> X + sum(Xs).

% Tail recursion of sum
sumOptimized(Xs) -> sumOptimized(Xs, 0).

sumOptimized([], S) -> S;
sumOptimized([X | Xs], S) -> sumOptimized(Xs, X + S).

% Product of a list
multiply(Xs) -> multiply(Xs, 1).

multiply([], S) -> S;
multiply([X | Xs], S) -> multiply(Xs, X * S).

% Maximum of a list
maximum([X]) -> X;
maximum([X | Xs]) -> max(X, maximum(Xs)).

%take(0, "hello") = []
%take(4, "hello") = "hell"
-spec take(integer(), [T]) -> [T].

take(0, _Xs) -> [];
take(_N, []) -> [];
take(N, [X | Xs]) when N > 0 -> [X | take(N - 1, Xs)].

% Keeps first occurrences
nub([]) -> [];
nub([X | Xs]) -> [X | nub(removeAll(X, Xs))].

removeAll(_, []) -> [];
removeAll(X, [X | Xs]) -> removeAll(X, Xs);
removeAll(X, [Y | Xs]) -> [Y | removeAll(X, Xs)].

% Keeps last occurrences
bun([]) -> [];
bun([X | Xs]) ->
    case member(X, Xs) of
      true -> bun(Xs);
      false -> [X | bun(Xs)]
    end.

member(_, []) -> false;
member(X, [X | _Xs]) -> true;
member(X, [_Y | Xs]) -> member(X, Xs).

palindrome(Xs) -> palin(nocaps(nopunct(Xs))).

% Recursively check whether a given entry (X) of a list (X + Xs) contains punctuation.
% If it does, ignore the entry and continue recursing with the remainder (Xs).
% If it doesn't, keep the entry (X) and check the remaining list (Xs).
nopunct([]) -> [];
nopunct([X | Xs]) ->
    case member(X, "., ;:\t\n'\"") of
      true -> nopunct(Xs);
      false -> [X | nopunct(Xs)]
    end.

nocaps([]) -> [];
nocaps([X | Xs]) -> [nocap(X) | nocaps(Xs)].

% Note: Characters are numeric under the hood.
% Check whether a given character is capitalized.
% If capitalized, add 32 to get its lowercase equivalent (X + 32).
% Else we just return the character (X).
nocap(X) ->
    case $A =< X andalso X =< $Z of
      true -> X + 32;
      false -> X
    end.

palin(Xs) -> Xs == reverse(Xs).

reverse(Xs) -> shunt(Xs, []).

shunt([], Ys) -> Ys;
shunt([X | Xs], Ys) -> shunt(Xs, [X | Ys]).
