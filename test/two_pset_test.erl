-module('two_pset_test').

-include_lib("eunit/include/eunit.hrl").

new_test() ->
  ?assertEqual(empty(), two_pset:new()).

lookup_an_added_element_test() ->
  S = from([a], [b]),
  ?assert(two_pset:lookup(a, S)).

lookup_a_removed_element_test() ->
  S = from([a], [a, b]),
  ?assertNot(two_pset:lookup(a, S)).

add_new_element_test() ->
  S = empty(),
  SA = two_pset:add(a, S),
  ?assert(same_values([a], SA)),
  SB = two_pset:add(b, SA),
  ?assert(same_values([a,b], SB )).

add_same_element_twice_test() ->
  S = from([a], []),
  ?assert(same_values([a], two_pset:add(a, S))).

remove_element_test_() ->
  [
    remove_element_test(from([a], [a]), a, from([a], [])),
    remove_element_test(from([a], []),  b, from([a], [])),
    remove_element_test(from([a], [a]), a, from([a], [a]))
  ].

remove_element_test(R, E, S) ->
  ?_assertEqual(R, two_pset:remove(E, S)).

compare_two_sets_with_same_values_test() ->
  A = from([a, b], [a]),
  B = from([a, b], [a]),
  ?assert(two_pset:compare(A, B)).

compare_different_sets_test_() ->
  [
    compare_different_test(from([a], [a]), from([a, b], [a, b])),
    compare_different_test(from([a,b], [a, b]), from([a], [a])),
    compare_different_test(from([a], [a]), from([], [])),
    compare_different_test(from([], []), from([a], [a]))
  ].

compare_different_test(A, B) ->
  ?_assertNot(two_pset:compare(A, B)).

%%
%% Utils
%%
empty() ->
  {[], []}.

from(A, R) ->
  {A, R}.

same_values(L, {A, R}) ->
  lists:all(fun(X) -> lists:member(X, A) and not lists:member(X, R) end, L).