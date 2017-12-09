-module('two_pset').

-export([new/0, add/2, remove/2, lookup/2, compare/2, merge/2]).

new() ->
  {[], []}.

lookup(E, {A, R}) ->
  is_member(E, A) and not is_member(E, R).

add(E, S) ->
  case lookup(E, S) of
    true  -> S;
    _     -> {A, R} = S,
             {[E|A], R}
  end.

remove(E, S) ->
  case lookup(E, S) of
    true  -> {A, R} = S,
             {A, [E|R]};
    _     -> S
  end.

compare({A1, R1}, {A2, R2}) ->
  lists:all(fun(X) -> is_member(X, A2) end, A1) and
    lists:all(fun(X) -> is_member(X, R2) end, R1).

merge({A1, R1}, {A2, R2}) ->
  A = merge_lists(A1, A2),
  R = merge_lists(R1, R2),
  {A, R}.

is_member(E, L) ->
  lists:member(E, L).

merge_lists(L, []) ->
  L;
merge_lists(L, [H|T]) ->
  case is_member(H, L) of
    true  -> merge_lists(L, T);
    _     -> merge_lists([H|L], T)
  end.