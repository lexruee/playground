-module(sum).
-export([sum/1]).

sum(List) ->
  sumIter(0, List).

sumIter(Acc, []) -> Acc;
sumIter(Acc, [H | List]) -> sumIter(Acc + H, List).
