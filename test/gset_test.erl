-module(gset_test).

-include_lib("eunit/include/eunit.hrl").

new_test() ->
	?assertEqual([], gset:new()).

add_element_test() ->
  ?assert(same_values([a], gset:add(a, []))).

add_different_elements_test() ->
  ?assert(same_values([a, b], gset:add(b, [a]))).

add_same_element_twice_test() ->
  ?assert(same_values([a], gset:add(a, [a]))).

lookup_existent_element_test() ->
  ?assert(gset:lookup(a, [b,c,a])).

lookup_non_existent_element_test() ->
  ?assertNot(gset:lookup(a, [])),
  ?assertNot(gset:lookup(z, [b,c,a])).

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

merge_two_gsets_with_same_items_test() ->
  A = [1,2,3],
  B = [1,2,3],
  ?assert(same_values([1,2,3], gset:merge(A, B))).

merge_two_gsets_with_some_different_items_test() ->
  A = [1,2,3,4],
  B = [1,2,3,5],
  ?assert(same_values([1,2,3,4,5], gset:merge(A, B))).

merge_two_disjoined_gsets_test() ->
  A = [1,2,3],
  B = [4,5,6],
  ?assert(same_values([1,2,3,4,5,6], gset:merge(A, B))).

%%
%% Utils
%%
same_values(S, T) ->
  lists:all(fun(X) -> lists:member(X, T) end, S).