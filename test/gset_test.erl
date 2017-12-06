-module(gset_test).

-include_lib("eunit/include/eunit.hrl").

new_test() ->
	?assertEqual([], gset:new()).

add_element_test() ->
  ?assertEqual([a], gset:add(a, [])).

add_different_elements_test() ->
  ?assertEqual([a, b], gset:add(b, [a])).

add_same_element_twice_test() ->
  ?assertEqual([a], gset:add(a, [a])).

lookup_existent_element() ->
  ?assertNot(gset:lookup(a, [b,c,a])).

lookup_non_existent_element() ->
  ?assertNot(gset:lookup(a, [])).

compare_two_gset_test() ->
  A = [1,2,3],
  B = [1,2,3],
  ?assert(gset:compare(A, B)).

compare_two_different_gset_test() ->
  A = [1,2,3],
  B = [1,2,4],
  ?assertNot(gset:compare(A, B)).

compare_a_smaller_gset_with_bigger_one_test() ->
  A = [1,2],
  B = [1,2,4],
  ?assert(gset:compare(A, B)).

compare_a_bigger_gset_with_a_smaller_one_test() ->
  A = [1,2,3],
  B = [1,2],
  ?assertNot(gset:compare(A, B)).