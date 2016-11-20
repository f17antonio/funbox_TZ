-module(fbtz_test).

-include_lib("eunit/include/eunit.hrl").
-include("./include/fbtz_test.hrl").

run_test_()->
  [
    {setup,
      fun fbtz_setup/0,
      fun fbtz_cleanup/1,
      fun({FbtzConfig, EredisConfig}) ->
        {inorder,
          [
            {"check_generate_numbers", check_generate_nums(FbtzConfig)},
            {"check_filter_numbers", check_filter_nums()},
            {"check_generator", check_generator(FbtzConfig, EredisConfig)},
            {"check_filtrator", check_filtator(FbtzConfig, EredisConfig)},
            {"check_filter_numbers_from_db", check_filter_nums_from_db(EredisConfig)}
          ]
        }
      end
    }
  ].

fbtz_setup() ->
  {ok, FbtzConfig, EredisConfig} = get_config(),
  Connect = proplists:get_value(connect_test, EredisConfig),
  {ok, _} = eredis_connect:start_link(Connect),
  {ok, _} = rdb:flushdb(),
  {ok, _} = fbtz_calc_opps_sup:start_link(),
  {FbtzConfig, EredisConfig}.

fbtz_cleanup(_) ->
  ok.

check_generate_nums(FbtzConfig) ->
  MaxNValue = proplists:get_value(max_n_value, FbtzConfig),
  Value = fbtz_generator:generate_numbers(?test_generate_numbers_count, MaxNValue),
  ?_assert(length(Value) =:= ?test_generate_numbers_count).

check_filter_nums() ->
  [
    ?_assertMatch(?prime_numbers, fbtz_filtrator:filter_numbers(?prime_numbers)),
    ?_assertMatch([], fbtz_filtrator:filter_numbers(?noprime_numbers))
  ].

check_generator(FbtzConfig, EredisConfig) ->
  MaxNValue = proplists:get_value(max_n_value, FbtzConfig),
  OppsPerSec = proplists:get_value(numbers_per_second, FbtzConfig),
  QueueKey = proplists:get_value(queue_key, EredisConfig),
  process_flag(trap_exit, true),
  {ok, Pid} = fbtz_generator:start_link(MaxNValue, OppsPerSec, QueueKey),
  timer:sleep(?generator_check_time * 1000),
  exit(Pid, kill),
  {ok, BNumbers} = rdb:get_list_values(QueueKey, 0, -1),
  TotalOpps = ?generator_check_time * OppsPerSec,
  NumsLength =  length(BNumbers),
  ?_assert((min(NumsLength, TotalOpps) / max(NumsLength, TotalOpps)) > 1 - ?admissible_error).

check_filtator(FbtzConfig, EredisConfig) ->
  OppsPerSec = proplists:get_value(numbers_per_second, FbtzConfig),
  ResultSetKey = proplists:get_value(result_set_key, EredisConfig),
  QueueKey = proplists:get_value(queue_key, EredisConfig),
  process_flag(trap_exit, true),
  {ok, Pid} = fbtz_filtrator:start_link(OppsPerSec, ResultSetKey, QueueKey),
  timer:sleep(?filtrator_check_time * 1000),
  exit(Pid, kill),
  {ok, BNumbers} = rdb:get_list_values(QueueKey, 0, -1),
  ?_assert(length(BNumbers) / (OppsPerSec * ?filtrator_check_time)  < ?admissible_error).

check_filter_nums_from_db(EredisConfig) ->
  ResultSetKey = proplists:get_value(result_set_key, EredisConfig),
  {ok, BNumbers} = rdb:get_all_set_values(ResultSetKey),
  ?_assertMatch(BNumbers, fbtz_filtrator:filter_numbers(BNumbers)).

get_config() ->
  case file:consult(?config_path) of
    {ok, [Config]} ->
      FbtzConfig = proplists:get_value(fbtz, Config),
      EredisConfig = proplists:get_value(eredis, Config),
      {ok, FbtzConfig, EredisConfig};
    Reason -> error({wrong_config, Reason})
  end.
