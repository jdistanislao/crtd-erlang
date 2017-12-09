-module('twopset').

-export([new/0, add/2, remove/2, lookup/2, compare/2, merge/2]).

new() ->
  {sets:new(), sets:new()}.

lookup(E, {A, R}) ->
  sets:is_element(E, A) and not sets:is_element(E, R).

add(E, S) ->
  case lookup(E, S) of
    true  -> S;
    _     -> {A, R} = S,
             {sets:add_element(E, A), R}
  end.

remove(E, S) ->
  case lookup(E, S) of
    true  -> {A, R} = S,
             {A, sets:add_element(E, R)};
    _     -> S
  end.

compare({A1, R1}, {A2, R2}) ->
  sets:is_subset(A1, A2) and sets:is_subset(R1, R2).

merge({A1, R1}, {A2, R2}) ->
  A = sets:union(A1, A2),
  R = sets:union(R1, R2),
  {A, R}.