-module(orset).

-include_lib("crtd_records.hrl").

-export([new/0, add/2, remove/2, lookup/2, compare/2, merge/2]).

new() ->
  maps:new().

add(E, S) ->
  {A, R} = maps:get(E, S, {sets:new(), sets:new()}),
  maps:put(E, {sets:add_element(make_ref(), A), R}, S).

remove(E, S) ->
  case maps:is_key(E, S) of
    true  -> {A, R} = maps:get(E, S),
             maps:put(E, {A, sets:union(A, R)}, S);
    _     -> S
  end.

lookup(E, S) ->
  case maps:is_key(E, S) of
    true  -> is_element(maps:get(E, S));
    _     -> false
  end.

compare(_S1, _S2) ->
  erlang:error(not_implemented).

merge(_S1, _S2) ->
  erlang:error(not_implemented).

%%
%% Utils
%%
is_element({A, R}) ->
  case sets:size(sets:subtract(A, R)) of
    0 -> false;
    _ -> {A, R}
  end.