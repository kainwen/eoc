-module(interp).

-export([interp/1]).
-export_type([value/0]).

-type value() :: {value, integer()}.
-type env() :: env:env() 
	     | env:env(value()).

-include("ast.hrl").

-spec interp(program()) -> value().
interp(Prog) ->
    Exp = Prog#program.body,
    interp_exp(Exp, env:new()).

-spec interp_exp(exp(), env()) -> value().
interp_exp(#integer_const_exp{value=Value}, _Env) ->
    {value, Value};
interp_exp(#read_exp{}, _Env) ->
    Int = utils:read_from_stdin(),
    {value, Int};
interp_exp(#negative_exp{body=Exp}, Env) ->
    {value, V} = interp_exp(Exp, Env),
    {value, -V};
interp_exp(#plus_exp{left=E1, right=E2}, Env) ->
    {value, V1} = interp_exp(E1, Env),
    {value, V2} = interp_exp(E2, Env),
    {value, V1+V2};
interp_exp(#sub_exp{left=E1, right=E2}, Env) ->
    {value, V1} = interp_exp(E1, Env),
    {value, V2} = interp_exp(E2, Env),
    {value, V1-V2};
interp_exp(#var_exp{name=Name}, Env) ->
    case env:find(Name, Env) of
	{ok, Val} -> Val;
	fail ->
	    erlang:error({"cannot find var", Name})
    end;
interp_exp(#let_exp{bindings=Bindings, body=Body}, Env) ->
    NVs = [{Name, interp_exp(Exp, Env)}
	   || {Name, Exp} <- Bindings],
    NewEnv = env:extend(NVs, Env),
    interp_exp(Body, NewEnv).
