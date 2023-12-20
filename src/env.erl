-module(env).

-include("types.hrl").

-export([find/2, extend/2, empty/0]).

-spec empty() -> env().
empty() ->
    [].

-spec find(atom(), env()) -> {ok, value()} | false.
find(Name, Env) ->
    case lists:keyfind(Name, 1, Env) of
	{Name, Value} ->
	    {ok, Value};
	false -> false
    end.

-spec extend([{atom(), value()}], env()) -> env().
extend(Bindings, Env) ->
    lists:append(Bindings, Env).
