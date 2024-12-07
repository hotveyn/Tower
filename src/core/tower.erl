-module(tower).

-export([insert/2, lookup/1, delete/1]).

insert(Key, Value) ->
    case tower_store:lookup(Key) of
        {ok, Pid} ->
            ok = tower_gen:replace(Pid, Value),
            ok;
        {error, not_found} ->
            {ok, Pid} = tower_gen:create(Value),
            tower_store:insert(Key, Pid),
            ok
    end.

lookup(Key) ->
    case tower_store:lookup(Key) of
        {ok, Pid} ->
            {ok, Value} = tower_gen:fetch(Pid),
            {ok, Value};
        {error, not_found} ->
            {error, not_found}
    end.

delete(Key) ->
    case tower_store:lookup(Key) of
        {ok, Pid} ->
            ok = tower_gen:delete(Pid),
            ok;
        {error, not_found} ->
            {error, not_found}
    end.
