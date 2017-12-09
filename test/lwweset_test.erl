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

%%
%% Utils
%%
empty() ->
  #lwweset{bias = add, elements = maps:new()}.