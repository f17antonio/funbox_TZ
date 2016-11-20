-module(rdb).

-compile([{nowarn_unused_function, [{ flushdb, 0 }]}]).

-export([
  set_list_values/2,
  get_list_values/3,
  trim_list/3,
  set_set_values/2,
  get_all_set_values/1
]).

-define(RCLIENT, eredis_client).

set_list_values(_, []) ->
  {ok, empty_values};
set_list_values(Key, Values) ->
  eredis:q(?RCLIENT, ["RPUSH", Key | Values]).

get_list_values(Key, FirstPos, LastPos) ->
  eredis:q(?RCLIENT, ["LRANGE", Key, FirstPos, LastPos]).

trim_list(Key, FirstPos, LastPos) ->
  eredis:q(?RCLIENT, ["LTRIM", Key, FirstPos, LastPos]).

set_set_values(_, []) ->
  {ok, empty_values};
set_set_values(Key, Values) ->
  eredis:q(?RCLIENT, ["SADD", Key | Values]).

get_all_set_values(Key) ->
  eredis:q(?RCLIENT, ["SMEMBERS", Key]).

flushdb() ->
  eredis:q(?RCLIENT, ["FLUSHDB"]).
