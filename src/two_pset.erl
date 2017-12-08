-module('two_pset').

-export([new/0, add/2, remove/2, lookup/2, compare/2, merge/2]).

new() ->
  {[], []}.

lookup(E, {A, R}) ->
  lists:member(E, A) and not lists:member(E, R).

add(E, S) ->
  case lookup(E, S) of
    true -> S;
    _    -> {A, R} = S,
            {[E|A], R}
  end.

remove(_E, _S) ->
  erlang:error(not_implemented).

compare(_S1, _S2) ->
  erlang:error(not_implemented).

merge(_S1, _S2) ->
  erlang:error(not_implemented).