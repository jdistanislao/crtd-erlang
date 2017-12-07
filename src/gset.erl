-module(gset).

-export([new/0, add/2, lookup/2, compare/2, merge/2]).

new() ->
  sets:new().

add(Element, Set) ->
  sets:add_element(Element, Set).

lookup(Element, Set) ->
  sets:is_element(Element, Set).

compare(Set1, Set2) ->
  sets:is_subset(Set1, Set2).

merge(Set1, Set2) ->
  sets:union(Set1, Set2).



