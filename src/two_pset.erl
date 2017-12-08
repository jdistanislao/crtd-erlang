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
    lists:all(fun(X) -> is_member(X, R2) end, R1) and
    lists:all(fun(X) -> is_member(X, A1) end, A2) and
    lists:all(fun(X) -> is_member(X, R1) end, R2).

merge(_S1, _S2) ->
  erlang:error(not_implemented).

is_member(E, L) ->
  lists:member(E, L).