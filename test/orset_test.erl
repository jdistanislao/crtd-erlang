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
  ?assertNot(orset:lookup(b, S)).

lookup_a_removed_element_test() ->
  S = orset:add(a, orset:new()),
  SR = orset:remove(a, S),
  ?assertNot(orset:lookup(a, SR)).

compare_same_sets_test() ->
  S1 = orset:add(a, orset:new()),
  S2 = orset:add(a, orset:new()),
  ?assert(orset:compare(S1, S2)).

compare_sets_with_full_intersections_test() ->
  S1 = orset:add(a, orset:new()),
  S2 = orset:add(a, orset:new()),
  S2b = orset:add(b, S2),
  ?assert(orset:compare(S1, S2b)).

compare_sets_with_intersections_test() ->
  S1 = orset:add(a, orset:new()),
  S1b = orset:add(b, S1),
  S2 = orset:add(a, orset:new()),
  ?assertNot(orset:compare(S1b, S2)).

compare_disjoined_sets_test() ->
  S1 = orset:add(a, orset:new()),
  S2 = orset:add(b, orset:new()),
  ?assertNot(orset:compare(S1, S2)).

merge_different_sets_test() ->
  S1 = orset:add(a, orset:new()),
  S1ar = orset:remove(a, S1),
  S1b = orset:add(a, S1ar),
  S2 = orset:add(b, orset:new()),
  S2ar = orset:remove(b, S2),
  S2b = orset:add(b, S2ar),
  S = orset:merge(S1b, S2b),
  {AA, AR} = maps:get(a, S),
  {BA, BR} = maps:get(b, S),
  ?assertEqual(2, sets:size(AA)),
  ?assertEqual(1, sets:size(AR)),
  ?assertEqual(2, sets:size(BA)),
  ?assertEqual(1, sets:size(BR)).

merge_same_sets_test() ->
  S1 = orset:add(a, orset:new()),
  S1ar = orset:remove(a, S1),
  S1b = orset:add(a, S1ar),
  S2 = orset:add(a, orset:new()),
  S2ar = orset:remove(a, S2),
  S2b = orset:add(a, S2ar),
  S = orset:merge(S1b, S2b),
  {AA, AR} = maps:get(a, S),
  ?assertEqual(4, sets:size(AA)),
  ?assertEqual(2, sets:size(AR)).

merge_sets_with_intersections_test() ->
  S1 = orset:add(a, orset:new()),
  S1ar = orset:remove(a, S1),
  S1b = orset:add(b, S1ar),
  S2 = orset:add(b, orset:new()),
  S2ar = orset:remove(b, S2),
  S2b = orset:add(c, S2ar),
  S = orset:merge(S1b, S2b),
  {AA, AR} = maps:get(a, S),
  {BA, BR} = maps:get(b, S),
  {CA, CR} = maps:get(c, S),
  ?assertEqual(1, sets:size(AA)),
  ?assertEqual(1, sets:size(AR)),
  ?assertEqual(2, sets:size(BA)),
  ?assertEqual(1, sets:size(BR)),
  ?assertEqual(1, sets:size(CA)),
  ?assertEqual(0, sets:size(CR)).
