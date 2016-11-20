-define(config_path, "../etc/fbtz.config").

-define(test_generate_numbers_count, 1547).
-define(admissible_error, 0.01).
-define(generator_check_time, 4). %seconds
-define(filtrator_check_time, 4). %seconds

-define(prime_numbers, [
  <<"1">>,
  <<"2">>,
  <<"5">>,
  <<"7">>,
  <<"11">>,
  <<"5791">>,
  <<"23743">>,
  <<"104059">>,
  <<"15485863">>,
  <<"587979773">>,
  <<"611901923">>,
  <<"860298839">>,
  <<"918662279">>,
  <<"2932031007403">>
]).

-define(noprime_numbers, [
  <<"4">>,
  <<"49">>,
  <<"100">>,
  <<"8946">>,
  <<"15485865">>,
  <<"860298841">>,
  <<"918662280">>,
  <<"2932031007401">>
]).
