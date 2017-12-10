-module(orset_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/crtd_records.hrl").


new_test() ->
  ?assertEqual(maps:new(), orset:new()).

add_new_element_test() ->
  SAdd = orset:add(a, orset:new()),
  {A, _} = maps:get(a, SAdd),
  ?assertEqual(1, sets:size(A)).

add_new_element_twice_test() ->
  S = orset:add(a, orset:new()),
  SAdd = orset:add(a, S),
  {A, _} = maps:get(a, SAdd),
  ?assertEqual(2, sets:size(A)).

add_different_elements_test() ->
  S = orset:add(a, orset:new()),
  SAdd = orset:add(b, S),
  {AA, _} = maps:get(a, SAdd),
  {BB, _} = maps:get(b, SAdd),
  ?assertEqual(1, sets:size(AA)),
  ?assertEqual(1, sets:size(BB)).

remove_element_test() ->
  S = orset:add(a, orset:new()),
  SR = orset:remove(a, S),
  {A, R} = maps:get(a, SR),
  ?assertEqual(A, R).

remove_element_twice_test() ->
  S = orset:add(a, orset:new()),
  SR1 = orset:remove(a, S),
  SR = orset:remove(a, SR1),
  {A, R} = maps:get(a, SR),
  ?assertEqual(A, R).

remove_a_non_existent_element_test() ->
  S = orset:add(a, orset:new()),
  SR = orset:remove(b, S),
  {A, R} = maps:get(a, SR),
  ?assertEqual(1, sets:size(A)),
  ?assertEqual(0, sets:size(R)).

lookup_an_added_element_test() ->
  S = orset:add(a, orset:new()),
  ?assertNotEqual(false, orset:lookup(a, S)).

lookup_a_non_existent_element_test() ->
  S = orset:add(a, orset:new()),
  ?assertEqual(false, orset:lookup(b, S)).

lookup_a_removed_element_test() ->
  S = orset:add(a, orset:new()),
  SR = orset:remove(a, S),
  ?assertEqual(false, orset:lookup(a, SR)).