-module(lwweset).

-include_lib("lwwe_records.hrl").

-export([new/0, add/3, remove/3, lookup/2, compare/2, merge/2]).

-define(BIAS, add).

new() ->
  #lwweset{bias = ?BIAS, elements = maps:new()}.

add(E, T, S) ->
  update(E, T, S, fun get_added_timestamp/2).

remove(E, T, S) ->
  update(E, T, S, fun get_removed_timestamp/2).

lookup(E, #lwweset{bias = B, elements = M}) ->
  case maps:is_key(E, M) of
    true  -> exists(maps:get(E, M), B);
    _     -> false
  end.

compare(#lwweset{elements = M1}, #lwweset{elements = M2}) ->
  M = maps:without(maps:keys(M2), M1),
  maps:size(M) == 0.

merge(#lwweset{elements = M1}, #lwweset{elements = M2}) ->
  M1Keys = maps:keys(M1),
  M2Keys = maps:keys(M2),
  Keys = sets:intersection(sets:from_list(M1Keys), sets:from_list(M2Keys)),
  GreaterFn = fun(X, MAcc) ->
    {A1, R1} = maps:get(X, M1),
    {A2, R2} = maps:get(X, M2),
    maps:put(X, {greater(A1, A2), greater(R1, R2)}, MAcc) end,
  MSame = sets:fold(GreaterFn, maps:new(), Keys),
  MDifferent = maps:merge(maps:without(M2Keys, M1), maps:without(M1Keys, M2)),
  #lwweset{bias = ?BIAS, elements = maps:merge(MSame, MDifferent)}.

%%
%% Utils
%%

update(E, T, S = #lwweset{elements = M}, TimestampFn) ->
  Timestamp = maps:get(E, M, {T, 0}),
  NewTimestamp = case maps:is_key(E, M) of
                   true  -> TimestampFn(T, Timestamp);
                   _     -> Timestamp
                 end,
  S#lwweset{elements = maps:put(E, NewTimestamp, M)}.

get_added_timestamp(T, {A, R}) when T > A ->
  {T, R};
get_added_timestamp(_, {A, R})->
  {A, R}.

get_removed_timestamp(T, {A, R}) when T > R ->
  {A, T};
get_removed_timestamp(_, {A, R})->
  {A, R}.

exists({A, R}, _) when A > R ->
  true;
exists({A, R}, _) when A < R ->
  false;
exists({A, R}, ?BIAS) when A == R ->
  true;
exists(_, _)  ->
  erlang:error(not_implemented).

greater(A, B) ->
  case A >= B of
    true  -> A;
    _     -> B
  end.
