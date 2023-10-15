-module(senv).

-export([empty_senv/0, extend_senv/2, apply_senv/2]).

-export_type([senv/0, scope/0]).

-type senv() :: empty_senv
              | {extend_senv, scope(), Saved_senv::senv()}.

-type scope() :: [{atom(), integer()}].


-spec empty_senv() -> senv().
empty_senv() -> empty_senv.

-spec extend_senv(senv(), [atom()]) -> senv().
extend_senv(Senv, Vars) ->
    Scope = [{V, name_server:get_name(V)} || V <- Vars],
    {extend_senv, Scope, Senv}.

-spec apply_senv(senv(), atom()) -> integer().
apply_senv(empty_senv, V) -> erlang:error({var_not_found, V});
apply_senv({extend_senv, Scope, Saved_senv}, V) ->
    case proplists:get_value(V, Scope) of
        undefined ->
            apply_senv(Saved_senv, V);
        N -> N
    end.
