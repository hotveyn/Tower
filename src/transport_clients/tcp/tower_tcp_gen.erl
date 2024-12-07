%%%====================================================================
%%% @author Ioann Shestopalov <ioann.shestopalov@gmail.com>
%%%  [https://github.com/hotveyn]
%%% @doc RPC over TCP server. This module defines a server process that
%%%      listens for incoming TCP connections and allows the user to
%%%      execute RPC commands via that TCP stream.
%%% @end
%%%====================================================================

-module(tower_tcp_gen).
-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/1, stop/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_info/2,
    handle_call/3,
    handle_cast/2,
    terminate/2,
    code_change/3
]).

-define(DEFAULT_PORT, 1055).
-define(REGEXP,
    element(2, re:compile("^method=(insert|lookup|delete);key=[a-zA-Z0-9]+;value=[a-zA-Z0-9]+$"))
).

-record(state, {lsock, request_count = 0}).

%%%====================================================================
%%% API
%%%====================================================================

%% @doc Start the server.
-spec start_link(string(), integer()) -> {ok, pid()}.
start_link(Server, Port) ->
    gen_server:start_link({local, Server}, ?MODULE, [Port], []).

%% @doc Calls "start_link(Server, Port)." using the default port
-spec start_link(string()) -> {ok, pid()}.
start_link(Server) ->
    start_link(Server, ?DEFAULT_PORT).

%% @doc Stop the server.
-spec stop(string()) -> {ok}.
stop(Server) ->
    gen_server:cast(Server, stop).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{lsock = LSock}, 0}.

handle_call(_, From, State) ->
    {reply, From, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({tcp, Socket, RawData}, #state{request_count = RequestCount} = State) ->
    io:format("handle_info tcp"),
    do_rpc(Socket, RawData),
    {noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock = LSock} = State) ->
    io:format("handle_info timeout"),
    {ok, _Sock} = gen_tcp:accept(LSock),
    io:format("handle_info timeout gen_tcp:accept"),
    {noreply, State}.

terminate(_Reasone, #state{lsock = LSock} = _State) ->
    case LSock of
        undefined ->
            ok;
        _ ->
            gen_tcp:close(LSock),
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal functions
%%%====================================================================

do_rpc(Socket, RawData) ->
    % try
    Message = re:replace(RawData, "\r\n$", "", [{return, list}]),
    case re:run(Message, ?REGEXP) of
        {match, _} ->
            [Method, Key, Value] = extract_keywords(
                Message
            ),
            Result = do_tower_method(Method, Key, Value),
            FormatedResult = format_result(Result),
            gen_tcp:send(
                Socket,
                io_lib:fwrite("~p~n", [
                    FormatedResult
                ])
            );
        nomatch ->
            gen_tcp:send(Socket, io_lib:fwrite("~p~n", ["error=VALIDATION_ERROR"]))
    end.

do_tower_method("insert", Key, Value) ->
    tower:insert(Key, Value);
do_tower_method("lookup", Key, _) ->
    tower:lookup(Key);
do_tower_method("delete", Key, _) ->
    tower:delete(Key).

format_result({error, not_found}) ->
    "result=error;value=not_found";
format_result({ok, Value}) ->
    "result=ok;value=" ++ Value;
format_result(ok) ->
    "result=ok".

extract_keywords(Message) ->
    PairsList = string:split(Message, ";", all),
    lists:map(
        fun(KeywordValue) ->
            [_, Value] = string:split(KeywordValue, "="),
            Value
        end,
        PairsList
    ).
