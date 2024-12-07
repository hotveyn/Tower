-module(tower_tcp_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 3,
        period => 1},

    ChildSpecifications = lists:map(fun ({Name, Port}) -> #{
            id => Name,
            start => {tower_tcp_gen, start_link, [Name, Port]},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000, % use 'infinity' for supervisor child
            type => worker, % worker | supervisor
            modules => [tower_tcp_gen]
        }
    end, [{tower_tcp_first, 8080},{tower_tcp_second, 8081}]),

    {ok, {SupervisorSpecification, ChildSpecifications}}.
