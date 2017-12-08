-module('two_pset_test').

-include_lib("eunit/include/eunit.hrl").

new_test() ->
  ?assertEqual(empty(), two_pset:new()).

lookup_an_added_element_test() ->
  S = from_lists([a], [b]),
  ?assert(two_pset:lookup(a, S)).

lookup_a_removed_element_test() ->
  S = from_lists([a], [a, b]),
  ?assertNot(two_pset:lookup(a, S)).

add_new_element_test() ->
  S = empty(),
  SA = two_pset:add(a, S),
  ?assert(same_values([a], SA)),
  SB = two_pset:add(b, SA),
  ?assert(same_values([a,b], SB )).

add_same_element_twice_test() ->
  S = from_lists([a], []),
  ?assert(same_values([a], two_pset:add(a, S))).


%%
%% Utils
%%
empty() ->
  {[], []}.

from_lists(A, R) ->
  {A, R}.

same_values(L, {A, R}) ->
  lists:all(fun(X) -> lists:member(X, A) and not lists:member(X, R) end, L).