-module(fbtz_generator).

-export([
  start_link/3,
  init/4,
  generator_loop/1
]).

-record(state, {
  opps_per_sec = 0  :: non_neg_integer(),
  queue_key         :: atom(),
  max_n_value = 0   :: non_neg_integer(),
  calc_opps_srv_pid :: pid()
}).

-define(timeout, 1000).

start_link(MaxNValue, OppsPerSec, QueueKey) ->
  proc_lib:start_link(?MODULE, init, [self(), MaxNValue, OppsPerSec, QueueKey]).

init(Parent, MaxNValue, OppsPerSec, QueueKey) ->
  {ok, CalcOppsSrvPid} = fbtz_calc_opps_sup:start_calc_opps_srv(OppsPerSec),
  State  = #state{
    opps_per_sec = OppsPerSec,
    queue_key = QueueKey,
    calc_opps_srv_pid = CalcOppsSrvPid,
    max_n_value = MaxNValue
  },
  proc_lib:init_ack(Parent, {ok, self()}),
  generator_loop(State).

generator_loop(State) ->
  {ok, OppsCount} = gen_server:call(State#state.calc_opps_srv_pid, {get_opps_count}),
  ListOfNumbers = generate_numbers(OppsCount, State#state.max_n_value),
  {ok, _} = rdb:set_list_values(State#state.queue_key, ListOfNumbers),
  check_time_error(OppsCount, State#state.opps_per_sec),
  generator_loop(State).

generate_numbers(Count, MaxNValue) ->
  generate_numbers(Count, MaxNValue, []).
generate_numbers(0, _, List) ->
  List;
generate_numbers(Count, MaxNValue, List) ->
  generate_numbers(Count - 1, MaxNValue, [random:uniform(MaxNValue) | List]).

check_time_error(OppsCount, OppsPerSec) when OppsCount / OppsPerSec > 10 ->
  lager:error("Generator is executed too slow (Shutdown). Opps Count: ~p~n", [OppsCount]),
  exit({shutdown, too_slow});
check_time_error(OppsCount, OppsPerSec) when OppsCount / OppsPerSec > 3 ->
  lager:warning("Generator is executed slow. Maybe redis work too slow. Opps Count: ~p~n", [OppsCount]);
check_time_error(_, _) ->
  {ok, no_errors}.
