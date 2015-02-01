-module(map).
-export([map/2]).

map(Fun, List) ->
  mapIter([], Fun, List).

mapIter(NewList, _, []) -> NewList;
mapIter(NewList, Fun, [H | List]) -> mapIter(NewList ++ [Fun(H)], Fun, List).
