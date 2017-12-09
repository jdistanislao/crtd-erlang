-module(lwweset_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/lwwe_records.hrl").

new_test() ->
  ?assertEqual(empty(), lwweset:new()).

add_new_element_test() ->
  S = empty(),
  #lwweset{elements = E} = lwweset:add(a, 1, S),
  ?assertEqual({1, 0}, maps:get(a, E)).

add_same_element_twice_with_same_timestamp_test() ->
  S = lwweset:add(a, 1, empty()),
  #lwweset{elements = E} = lwweset:add(a, 1, S),
  ?assertEqual({1, 0}, maps:get(a, E)).

add_same_element_twice_with_newer_timestamp_test() ->
  S = lwweset:add(a, 1, empty()),
  #lwweset{elements = E} = lwweset:add(a, 2, S),
  ?assertEqual({2, 0}, maps:get(a, E)).

add_same_element_twice_with_older_timestamp_test() ->
  S = lwweset:add(a, 2, empty()),
  #lwweset{elements = E} = lwweset:add(a, 1, S),
  ?assertEqual({2, 0}, maps:get(a, E)).

remove_element_test() ->
  S = lwweset:add(a, 1, empty()),
  #lwweset{elements = E} = lwweset:remove(a, 1, S),
  ?assertEqual({1, 1}, maps:get(a, E)).

remove_element_with_same_timestamp_test() ->
  SOld = lwweset:add(a, 1, empty()),
  S = lwweset:remove(a, 1, SOld),
  #lwweset{elements = E} = lwweset:remove(a, 1, S),
  ?assertEqual({1, 1}, maps:get(a, E)).

remove_element_with_newer_timestamp_test() ->
  SOld = lwweset:add(a, 1, empty()),
  S = lwweset:remove(a, 1, SOld),
  #lwweset{elements = E} = lwweset:remove(a, 2, S),
  ?assertEqual({1, 2}, maps:get(a, E)).

remove_element_with_older_timestamp_test() ->
  SOld = lwweset:add(a, 1, empty()),
  S = lwweset:remove(a, 2, SOld),
  #lwweset{elements = E} = lwweset:remove(a, 1, S),
  ?assertEqual({1, 2}, maps:get(a, E)).

lookup_an_existent_element_test() ->
  SAdd = lwweset:add(a, 1, empty()),
  SRemove = lwweset:remove(a, 2, SAdd),
  S = lwweset:add(a, 3, SRemove),
  ?assert(lwweset:lookup(a,  S)).

lookup_a_non_existent_element_test() ->
  S = lwweset:add(a, 1, empty()),
  ?assertNot(lwweset:lookup(b,  S)).

lookup_a_deleted_element_test() ->
  SOld = lwweset:add(a, 1, empty()),
  S = lwweset:remove(a, 2, SOld),
  ?assertNot(lwweset:lookup(a,  S)).

lookup_a_biased_element_test() ->
  SOld = lwweset:add(a, 1, empty()),
  S = lwweset:remove(a, 1, SOld),
  ?assert(lwweset:lookup(a,  S)).

compare_same_sets_test() ->
  S1 = lwweset:add(a, 1, empty()),
  S2 = lwweset:add(a, 1, empty()),
  ?assert(lwweset:compare(S1, S2)),
  S3 = lwweset:add(a, 1, empty()),
  S4 = lwweset:add(a, 2, empty()),
  ?assert(lwweset:compare(S3, S4)),
  S5 = lwweset:add(a, 1, empty()),
  S6a = lwweset:add(a, 2, empty()),
  S6b = lwweset:add(b, 1, S6a),
  ?assert(lwweset:compare(S5, S6b)).

compare_different_sets_test() ->
  S1 = lwweset:add(a, 1, empty()),
  S2 = lwweset:add(b, 1, empty()),
  ?assertNot(lwweset:compare(S1, S2)),
  S3a = lwweset:add(a, 1, empty()),
  S3b = lwweset:add(b, 1, S3a),
  S4 = lwweset:add(b, 1, empty()),
  ?assertNot(lwweset:compare(S3b, S4)).

%%
%% Utils
%%
empty() ->
  #lwweset{bias = add, elements = maps:new()}.