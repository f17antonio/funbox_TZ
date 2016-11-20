-module(fbtz_calc_opps_srv).
-behaviour(gen_server).

-export([start_link/1]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  opps_per_sec = 0        :: non_neg_integer(),
  opps_per_mcsec = 0      :: float(),
  last_opps_remainder = 0 :: float(),
  last_get_time = 0       :: non_neg_integer()
}).

-define(microseconds, 1000000).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(OppsPerSec) ->
  {ok, #state{
    opps_per_sec = OppsPerSec,
    opps_per_mcsec = OppsPerSec / ?microseconds,
    last_get_time = erlang:system_time(?microseconds)
  }}.

handle_call({get_opps_count}, _From, State) ->
  NowTime = erlang:system_time(?microseconds),
  DeltaTime = NowTime - State#state.last_get_time,
  {ok, OppsCount, OppsRem} = get_opps_count(DeltaTime, State#state.last_opps_remainder, State#state.opps_per_mcsec),
  {reply, {ok, OppsCount}, State#state{last_opps_remainder = OppsRem, last_get_time = NowTime}};
handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_opps_count(DeltaTime, LastOppsRemainder, OppsPerMcSec) ->
  Count = DeltaTime * OppsPerMcSec + LastOppsRemainder,
  TruncCount = trunc(Count),
  {ok, TruncCount, Count - TruncCount}.

