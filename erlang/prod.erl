-module(prod).
-export([prod/1]).

prod(List) ->
  prodIter(1, List).

prodIter(Acc, []) -> Acc;
prodIter(Acc, [H | List]) -> prodIter(Acc * H, List).
