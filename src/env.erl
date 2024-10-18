-module(env).

-export([find/2, new/0, extend/2]).
-export_type([env/1, env/0]).

-type key() :: atom().

-opaque env(Val) :: [{key(), Val}].
-opaque env() :: [].

-spec new() -> env().
new() ->
    [].

-spec find(key(), env()|env(Val)) -> {ok, Val} | fail.
find(_Key, []) -> fail;
find(Key, [{K, V}|Env]) ->
    case K =:= Key of
	true ->
	    {ok, V};
	false ->
	    find(Key, Env)
    end.

-spec extend([{key(), Val}], env()|env(Val)) -> env()|env(Val).
extend([], Env) -> Env;
extend([{Key, Val}|NVs], Env) ->
    extend(NVs, [{Key, Val}|Env]).
