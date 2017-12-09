-module(lwweset).

-include_lib("lwwe_records.hrl").

-export([new/0, add/3, remove/3, lookup/2, compare/2, merge/2]).


new() ->
  #lwweset{bias = add, elements = maps:new()}.

add(E, T, S) ->
  update(E, T, S, fun get_added_timestamp/2).

remove(E, T, S) ->
  update(E, T, S, fun get_removed_timestamp/2).

lookup(_E, _S) ->
  erlang:error(not_implemented).

compare(_S1, _S2) ->
  erlang:error(not_implemented).

merge(_S1, _S2) ->
  erlang:error(not_implemented).

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