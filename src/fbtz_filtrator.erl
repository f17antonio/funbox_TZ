-module(fbtz_filtrator).

-export([
  start_link/3,
  init/4,
  filtrator_loop/1
]).

-record(state, {
  opps_per_sec = 0 :: non_neg_integer(),
  queue_key        :: atom(),
  result_set_key   :: atom(),
  missing_time = 0 :: non_neg_integer()
}).

-define(miliseconds, 1000).

start_link(OppsPerSec, ResultSetKey, QueueKey) ->
  proc_lib:start_link(?MODULE, init, [self(), OppsPerSec, ResultSetKey, QueueKey]).

init(Parent, OppsPerSec, ResultSetKey, QueueKey) ->
  State = #state{
    opps_per_sec = OppsPerSec,
    queue_key = QueueKey,
    result_set_key = ResultSetKey
  },
  proc_lib:init_ack(Parent, {ok, self()}),
  filtrator_loop(State).

filtrator_loop(State) ->
  StartTime = erlang:system_time(?miliseconds),
  {ok, BNumbers} = rdb:get_list_values(State#state.queue_key, 0, State#state.opps_per_sec - 1),
  NumbersLength = length(BNumbers),
  {ok, _} = rdb:trim_list(State#state.queue_key, NumbersLength, -1),
  FilteredNumbers = filter_numbers(BNumbers),
  {ok, _} = rdb:set_set_values(State#state.result_set_key, FilteredNumbers),
  EndTime = erlang:system_time(?miliseconds),
  NewState = check_execute_time(EndTime - StartTime, NumbersLength, State),
  filtrator_loop(NewState).

filter_numbers(BNumbers) ->
  [BNumber || BNumber <- BNumbers, is_prime_number(BNumber) == true].

is_prime_number(<<"1">>) ->
  true;
is_prime_number(<<"2">>) ->
  true;
is_prime_number(BNumber) ->
  Number = binary_to_integer(BNumber),
  is_prime_number(Number, 2, erlang:trunc(math:sqrt(Number)) + 1).

is_prime_number(_, I, Max) when I >= Max ->
  true;
is_prime_number(N, I, _) when N rem I =:= 0 ->
  false;
is_prime_number(N, 2, Max) ->
  is_prime_number(N, 3, Max);
is_prime_number(N, I, Max) ->
  is_prime_number(N, I + 2, Max).

check_execute_time(ExecuteTime, _, State) when ExecuteTime > ?miliseconds ->
  MissingTime = ExecuteTime - ?miliseconds + State#state.missing_time,
  check_time_error(MissingTime),
  State#state{missing_time = MissingTime};
check_execute_time(ExecuteTime, Length, State) ->
  AddTime = ?miliseconds - (ExecuteTime + State#state.missing_time),
  if
    AddTime > 0, Length < State#state.opps_per_sec ->
      timer:sleep(AddTime),
      State#state{missing_time = 0};
    AddTime > 0 ->
      State#state{missing_time = 0};
    true ->
      MissingTime = -AddTime,
      State#state{missing_time = MissingTime}
  end.

check_time_error(MissingTime) when MissingTime / ?miliseconds >= 10 ->
  lager:error("Filtarator is executed too slow (Shutdown). Maybe max_n_value is too large. Missing Time: ~p~n", [MissingTime]),
  exit({shutdown, too_slow});
check_time_error(MissingTime) when MissingTime / ?miliseconds >= 3 ->
  lager:warning("Filtarator is executed slow. Maybe max_n_value is too large. Missing Time: ~p~n", [MissingTime]);
check_time_error(_) ->
  {ok, no_errors}.
