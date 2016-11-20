-module(eredis_connect).

-export([
  start_link/1,
  stop/0
]).

start_link(ConnectData) ->
  {ok, connect(ConnectData)}.

connect(ConnectData) ->
  {ok, Client} = eredis:start_link(ConnectData),
  register(eredis_client, Client),
  Client.

stop() ->
  eredis:stop(eredis_client).
