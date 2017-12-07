-module(gset).

-export([new/0, add/2, lookup/2, compare/2, merge/2]).

new() ->
  [].

add(Element, GSet) ->
  case lookup(Element, GSet) of
    true -> GSet;
    _    -> lists:reverse([Element | GSet])
  end.

lookup(Element, GSet) ->
	lists:member(Element, GSet).

compare(GSet1, GSet2) ->
  lists:all(fun(X) -> lookup(X, GSet2) end, GSet1).

merge(Gset, []) ->
  Gset;
merge(GSet1, [H|T]) ->
  merge(add(H, GSet1), T).



