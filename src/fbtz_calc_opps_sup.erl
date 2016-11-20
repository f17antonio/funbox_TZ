-module(fbtz_calc_opps_sup).
-behaviour(supervisor).

-export([
  start_link/0,
  start_calc_opps_srv/1,
  terminate_calc_opps_srv/1
]).

-export([
  init/1
]).

-define(child(I), {I, {I, start_link, []}, transient, 2000, worker, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_calc_opps_srv(Args) ->
  supervisor:start_child(?MODULE, [Args]).

terminate_calc_opps_srv(Pid) ->
  supervisor:terminate_child(?MODULE, Pid).

init(_Args) ->
  Child_Spec = ?child(fbtz_calc_opps_srv),
  {ok,{{simple_one_for_one, 1000, 60}, [ Child_Spec ]}}.
