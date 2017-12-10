-module(orset).

-include_lib("crtd_records.hrl").

-export([new/0, add/2, remove/2, lookup/2, compare/2, merge/2]).

new() ->
  erlang:error(not_implemented).

add(_E, _S) ->
  erlang:error(not_implemented).

remove(_E, _S) ->
  erlang:error(not_implemented).

lookup(_E, _S) ->
  erlang:error(not_implemented).

compare(_S1, _S2) ->
  erlang:error(not_implemented).

merge(_S1, _S2) ->
  erlang:error(not_implemented).