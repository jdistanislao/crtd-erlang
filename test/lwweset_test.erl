-module(lwweset_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/lwwe_records.hrl").

new_test() ->
  ?assertEqual(empty(), lwweset:new()).

add_new_element_test() ->
  S = empty(),
  #lwweset{elements = E} = lwweset:add(a, 1, S),
  ?assertEqual({1, 0}, maps:get(a, E)).


%%
%% Utils
%%
empty() ->
  #lwweset{bias = add, elements = maps:new()}.