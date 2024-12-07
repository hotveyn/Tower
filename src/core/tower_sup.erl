-module(tower_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Value, TTL) ->
  supervisor:start_child(?MODULE, [Value, TTL]).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => simple_one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 0,
        period => 1},

    ChildSpecifications = [
        #{
            id => tower_gen,
            start => {tower_gen, start_link, []},
            restart => temporary, % permanent | transient | temporary
            shutdown => brutal_kill, % use 'infinity' for supervisor child
            type => worker, % worker | supervisor
            modules => [tower_gen]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.