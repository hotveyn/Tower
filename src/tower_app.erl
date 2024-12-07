-module(tower_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    tower_store:init(),
    % bootstrap supervisors
    {ok, _} = tower_sup:start_link(),
    {ok, _} = tower_tcp_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc This function is called whenever an application has stopped. It
%% is intended to be the opposite of `?MODULE:start/2' and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec stop(State :: term()) -> any().
stop(_State) ->
    ok.
