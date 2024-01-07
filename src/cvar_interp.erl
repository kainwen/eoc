-module(cvar_interp).

-include("types.hrl").

-export([interp/1]).

-spec interp(#cvar_prog{}) -> value().
interp(#cvar_prog{body=Body}) ->
    %% currently we should only have one block
    Tail = element(2, lists:nth(1, Body)),
    Env = env:empty(),
    interp_tail(Tail, Env).

-spec interp_tail(cvar_tail(), env()) -> value().
interp_tail(#cvar_return{exp=Exp}, Env) ->
    interp_exp(Exp, Env);
interp_tail(#cvar_seq{stmts=Stmts, tail=#cvar_return{exp=Exp}}, Env) ->
    NewEnv = interp_stmts(Stmts, Env),
    interp_exp(Exp, NewEnv).

-spec interp_exp(cvar_exp(), env()) -> value().
interp_exp(#cvar_read{}, _Env) ->
    I = utils:read_from_stdin(),
    #int_value{value=I};
interp_exp(#cvar_neg{arg=Exp}, Env) ->
    #int_value{value=I} = interp_atom(Exp, Env),
    #int_value{value=-I};
interp_exp(#cvar_plus{left=L, right=R}, Env) ->
    #int_value{value=LI} = interp_atom(L, Env),
    #int_value{value=RI} = interp_atom(R, Env),
    #int_value{value=LI+RI};
interp_exp(#cvar_sub{left=L, right=R}, Env) ->
    #int_value{value=LI} = interp_atom(L, Env),
    #int_value{value=RI} = interp_atom(R, Env),
    #int_value{value=LI-RI};
interp_exp(A, Env) ->
    interp_atom(A, Env).

-spec interp_atom(cvar_atm(), env()) -> value().
interp_atom(#cvar_int{value=I}, _Env) ->
    #int_value{value=I};
interp_atom(#cvar_var{var=Var}, Env) ->
    case env:find(Var, Env) of
	{ok, Value} ->
	    Value;
	false ->
	    erlang:error({"cannot find var", Var})
    end.

-spec interp_stmts([cvar_stmt()], env()) -> env().
interp_stmts([], Env) -> Env;
interp_stmts([#cvar_assign{var=Var, exp=Exp}|Stmts], Env) -> 
    Val = interp_exp(Exp, Env),
    NewEnv = env:extend([{Var, Val}], Env),
    interp_stmts(Stmts, NewEnv).
