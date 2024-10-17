-module(env).

-export([find/2, new/0, extend/2]).
-export_type([env/0]).

-type key() :: atom().
-type value() :: interp:value().

-opaque env() :: [{key(), value()}].

-spec new() -> env().
new() ->
    [].

-spec find(key(), env()) -> {ok, value()} | fail.
find(_Key, []) -> fail;
find(Key, [{K, V}|Env]) ->
    case K =:= Key of
	true ->
	    {ok, V};
	false ->
	    find(Key, Env)
    end.

-spec extend([{key(), value()}], env()) -> env().
extend([], Env) -> Env;
extend([{Key, Val}|NVs], Env) ->
    extend(NVs, [{Key, Val}|Env]).
