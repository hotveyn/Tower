-module(tower_gen).

-behaviour(gen_server).

%% API
-export([start_link/2, create/2, create/1, fetch/1, replace/2, delete/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%%===================================================================
%%% Includes, defines, types and records
%%%===================================================================

-define(DEFAULT_TTL, 60 * 60 * 24).

-record(state, {value, ttl, start_time}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(Value :: string(), TTL :: number()) ->
    {ok, Pid :: pid()}
    | {error, Error :: {already_started, pid()}}
    | {error, Error :: term()}
    | ignore.
start_link(Value, TTL) ->
    gen_server:start_link(?MODULE, [Value, TTL], []).

create(Value, TTL) ->
    tower_sup:start_child(Value, TTL).

create(Value) ->
    create(Value, ?DEFAULT_TTL).

fetch(Pid) ->
    gen_server:call(Pid, fetch).

replace(Pid, Value) ->
    gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
    gen_server:cast(Pid, delete).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Value, TTL]) ->
    Now = calendar:local_time(),
    StartTime = calendar:datetime_to_gregorian_seconds(Now),
    {
        ok,
        #state{
            value = Value,
            ttl = TTL,
            start_time = StartTime
        },
        time_left(StartTime, TTL)
    }.

time_left(_StarTime, infinity) ->
    infinity;
time_left(StartTime, TTL) ->
    Now = calendar:local_time(),
    CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
    TimeElapsed = CurrentTime - StartTime,
    case TTL - TimeElapsed of
        Time when Time =< 0 -> 0;
        Time -> Time * 1000
    end.

handle_call(fetch, _From, State) ->
    #state{
        value = Value,
        ttl = TTL,
        start_time = StartTime
    } = State,
    TimeLeft = time_left(StartTime, TTL),
    {reply, {ok, Value}, State, TimeLeft}.

handle_cast({replace, Value}, State) ->
    #state{
        ttl = TTL,
        start_time = StartTime
    } = State,
    TimeLeft = time_left(StartTime, TTL),
    {noreply, State#state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
    {stop, normal, State}.

handle_info(timeout, State) ->
    {stop, normal, State}.

terminate(_Reason, _State) ->
    tower_store:delete(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
