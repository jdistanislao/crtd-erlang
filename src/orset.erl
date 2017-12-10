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

compare(S1, S2) ->
  M = maps:without(maps:keys(S2), S1),
  case maps:size(M) of
    0 -> true;
    _ -> false
  end.

merge(S1, S2) ->
  M1Keys = maps:keys(S1),
  M2Keys = maps:keys(S2),
  Keys = maps:keys(maps:with(M2Keys, S1)),
  MergeFn = fun(X, Acc) ->
    {A1, R1} = maps:get(X, S1),
    {A2, R2} = maps:get(X, S2),
    maps:put(X, {sets:union(A1, A2), sets:union(R1, R2)}, Acc) end,
  MSame = lists:foldl(MergeFn, maps:new(), Keys),
  MDifferent = maps:merge(maps:without(M2Keys, S1), maps:without(M1Keys, S2)),
  maps:merge(MSame, MDifferent).

%%
%% Utils
%%
is_element({A, R}) ->
  case sets:size(sets:subtract(A, R)) of
    0 -> false;
    _ -> {A, R}
  end.