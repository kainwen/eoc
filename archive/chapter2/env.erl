-module(env).

-export([empty_env/0, extend_env/2, apply_env/2]).

-export_type([env/0]).

-type env() :: empty_env
             | {extend_env, [{atom(), integer()}], Saved_env::env()}.

-spec empty_env() -> env().
empty_env() -> empty_env.

-spec extend_env(env(), [{atom(), integer()}]) -> env().
extend_env(Env, Scope) ->
    {extend_env, Scope, Env}.

-spec apply_env(env(), atom()) -> integer().
apply_env(empty_env, Var) -> erlang:error({can_not_find_var, Var});
apply_env({extend_env, Scope, Saved_env}, Var) ->
    case proplists:get_value(Var, Scope) of
        undefined ->
            apply_env(Saved_env, Var);
        N -> N
    end.
