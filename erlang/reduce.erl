-module(reduce).
-export([reduce/3]).

reduce(Init,Fun,List) ->
  reduceIter(Init, Fun, List).

reduceIter(Acc, _, []) -> Acc;
reduceIter(Acc, Fun, [H | List]) -> reduceIter(Fun(Acc, H), Fun, List).
