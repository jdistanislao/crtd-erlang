-module(gset_test).

-include_lib("eunit/include/eunit.hrl").

new_test() ->
	?assert(same_values([], gset:new())).

add_element_test() ->
  S = create_from_list([]),
  ?assert(same_values([a], gset:add(a, S))).

add_different_elements_test() ->
  S = create_from_list([a]),
  ?assert(same_values([a, b], gset:add(b, S))).

add_same_element_twice_test() ->
  S = create_from_list([a]),
  ?assert(same_values([a], gset:add(a, S))).

lookup_existent_element_test() ->
  S = create_from_list([b,c,a]),
  ?assert(gset:lookup(a, S)).

lookup_non_existent_element_test() ->
  S1 = create_from_list([]),
  ?assertNot(gset:lookup(a, S1)),
  S2 = create_from_list([b,c,a]),
  ?assertNot(gset:lookup(z, S2)).

compare_two_gset_test() ->
  A = create_from_list([1,2,3]),
  B = create_from_list([1,2,3]),
  ?assert(gset:compare(A, B)).

compare_two_different_gset_test() ->
  A = create_from_list([1,2,3]),
  B = create_from_list([1,2,4]),
  ?assertNot(gset:compare(A, B)).

compare_a_smaller_gset_with_bigger_one_test() ->
  A = create_from_list([1,2]),
  B = create_from_list([1,2,4]),
  ?assert(gset:compare(A, B)).

compare_a_bigger_gset_with_a_smaller_one_test() ->
  A = create_from_list([1,2,3]),
  B = create_from_list([1,2]),
  ?assertNot(gset:compare(A, B)).

merge_two_gsets_with_same_items_test() ->
  A = create_from_list([1,2,3]),
  B = create_from_list([1,2,3]),
  ?assert(same_values([1,2,3], gset:merge(A, B))).

merge_two_gsets_with_some_different_items_test() ->
  A = create_from_list([1,2,3,4]),
  B = create_from_list([1,2,3,5]),
  ?assert(same_values([1,2,3,4,5], gset:merge(A, B))).

merge_two_disjoined_gsets_test() ->
  A = create_from_list([1,2,3]),
  B = create_from_list([4,5,6]),
  ?assert(same_values([1,2,3,4,5,6], gset:merge(A, B))).

%%
%% Utils
%%
create_from_list(L) ->
  sets:from_list(L).

same_values(L, S) ->
  lists:all(fun(X) -> sets:is_element(X, S) end, L).