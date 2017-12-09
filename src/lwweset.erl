-module(lwweset).

-include_lib("lwwe_records.hrl").

-export([new/0, add/3, remove/3, lookup/2, compare/2, merge/2]).


new() ->
  #lwweset{bias = add, elements = maps:new()}.

add(E, T, S = #lwweset{elements = M}) ->
  NewTimestamp = case maps:is_key(E, M) of
    true  -> get_add_timestamp(T, maps:get(E, M));
    _     -> {T, 0}
  end,
  S#lwweset{elements = maps:put(E, NewTimestamp, M)}.

remove(_E, _T, _S) ->
  erlang:error(not_implemented).

lookup(_E, _S) ->
  erlang:error(not_implemented).

compare(_S1, _S2) ->
  erlang:error(not_implemented).

merge(_S1, _S2) ->
  erlang:error(not_implemented).

get_add_timestamp(T, {A, R}) when T > A ->
  {T, R};
get_add_timestamp(_, {A, R})->
  {A, R}.