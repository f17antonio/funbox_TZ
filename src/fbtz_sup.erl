-module(fbtz_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_dynamic_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(child(I, Type, Args), {I, {I, start_link, Args}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
  {ok, EredisConnectData} = application:get_env(eredis, connect),
  {ok, QueueKey} = application:get_env(eredis, queue_key),
  {ok, ResultSetKey} = application:get_env(eredis, result_set_key),
  {ok, OppsPerSec} = application:get_env(fbtz, numbers_per_second),
  {ok, MaxNValue} = application:get_env(fbtz, max_n_value),

  FbtzGenerator = ?child(fbtz_generator, worker, [MaxNValue, OppsPerSec, QueueKey]),
  FbtzFiltrator = ?child(fbtz_filtrator, worker, [OppsPerSec, ResultSetKey, QueueKey]),
  Eredis = ?child(eredis_connect, worker, [EredisConnectData]),
  FbtzCalcOppsSup = ?child(fbtz_calc_opps_sup, supervisor, []),

  {ok, { {one_for_all, 5, 10}, [
    Eredis,
    FbtzCalcOppsSup,
    FbtzGenerator,
    FbtzFiltrator
  ]}}.

start_dynamic_child(Args) ->
  supervisor:start_child(?MODULE, [Args]).
