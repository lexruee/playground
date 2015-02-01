-module(fib).
-export([fib/1, fibList/2]).

fib(N) -> fibIter(0, 1, N).
fibIter(Fn_1, _, 0) -> Fn_1;
fibIter(Fn_1, Fn_2, N) -> fibIter(Fn_1 + Fn_2, Fn_1, N-1).

fibList(S, E) -> [ fib:fib(X) || X <- lists:seq(S,E,1) ].
