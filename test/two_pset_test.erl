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

add_element_test_() ->
  [
    add_element_test([a], a, empty()),
    add_element_test([a,b], b, from([a],[])),
    add_element_test([a], a, from([a],[]))
  ].

add_element_test(L, E, S) ->
  ?_assert(same_values(L, two_pset:add(E, S))).

remove_element_test_() ->
  [
    remove_element_test(from([a], [a]), a, from([a], [])),
    remove_element_test(from([a], []),  b, from([a], [])),
    remove_element_test(from([a], [a]), a, from([a], [a]))
  ].

remove_element_test(R, E, S) ->
  ?_assertEqual(R, two_pset:remove(E, S)).

compare_different_sets_test_() ->
  [
    compare_ok_different_test(from([a, b], [a]), from([a, b], [a])),
    compare_ok_different_test(from([a], [a]), from([a, b], [a,b])),
    compare_no_different_test(from([a,b], [a,b]), from([a], [a])),
    compare_no_different_test(from([a], [a]), from([], [])),
    compare_ok_different_test(from([], []), from([a], [a]))
  ].

compare_ok_different_test(A, B) ->
  ?_assert(two_pset:compare(A, B)).

compare_no_different_test(A, B) ->
  ?_assertNot(two_pset:compare(A, B)).

merge_two_sets_test_() ->
  [
    merge_two_sets_test(from([a], [a]), from([a], [a]), from([a], [a])),
    merge_two_sets_test(from([a,b], [b]), from([a], []), from([a,b], [b])),
    merge_two_sets_test(from([a,b], [b]), from([], []), from([a,b], [b])),
    merge_two_sets_test(from([a,b], [b]), from([a,b], [b]), from([], []))
  ].

merge_two_sets_test(E, S1, S2) ->
  ?_assert(same_members(E, two_pset:merge(S1, S2))).

%%
%% Utils
%%
empty() ->
  {[], []}.

from(A, R) ->
  {A, R}.

same_values(L, {A, R}) ->
  case length(lists:subtract(L, A)) of
    0 -> lists:all(fun(X) -> not lists:member(X, R) end, L);
    _ -> false
  end.

same_members({EA, ER}, {A, R}) ->
  LA = length(lists:subtract(EA, A)),
  LR = length(lists:subtract(ER, R)),
  0 == LA + LR.

