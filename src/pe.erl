-module(pe).

-export([partial_eval/1]).

-include("ast.hrl").

-type env() :: env:env() | env:env(exp()).

-spec partial_eval(program()) -> program().
partial_eval(#program{body=Body}) ->
    #program{body=pe_exp(Body, env:new())}.

-spec pe_exp(exp(), env()) -> exp().
pe_exp(Int=#integer_const_exp{}, _Env) -> Int;
pe_exp(R=#read_exp{}, _Env) -> R;
pe_exp(#negative_exp{body=Exp}, Env) ->
    NewExp = pe_exp(Exp, Env),
    case NewExp of
	#integer_const_exp{value=Val} ->
	    #integer_const_exp{value=-Val};
	_ ->
	    #negative_exp{body=NewExp}
    end;
pe_exp(#plus_exp{left=Left, right=Right}, Env) ->
    L = pe_exp(Left, Env),
    R = pe_exp(Right, Env),
    case {L, R} of
	{#integer_const_exp{value=V1},
	 #integer_const_exp{value=V2}} ->
	    #integer_const_exp{value=V1+V2};
	_ ->
	    #plus_exp{left=L, right=R}
    end;
pe_exp(#sub_exp{left=Left, right=Right}, Env) ->
    L = pe_exp(Left, Env),
    R = pe_exp(Right, Env),
    case {L, R} of
	{#integer_const_exp{value=V1},
	 #integer_const_exp{value=V2}} ->
	    #integer_const_exp{value=V1-V2};
	_ ->
	    #sub_exp{left=L, right=R}
    end;    
pe_exp(V=#var_exp{name=Var}, Env) ->
    case env:find(Var, Env) of
	{ok, Int=#integer_const_exp{}} -> Int;
	_ ->
	    V
    end;
pe_exp(#let_exp{bindings=Bindings, body=Body}, Env) ->
    BEs = [{Var, pe_exp(Exp, Env)} || {Var, Exp} <- Bindings],
    IntBindings = [{Var, Exp} 
		   || {Var, Exp} <- BEs, is_record(Exp, integer_const_exp)],
    OtherBindings = [{Var, Exp} 
		     || {Var, Exp} <- BEs, not is_record(Exp, integer_const_exp)],
    NewEnv = env:extend(IntBindings, Env),
    NewBody = pe_exp(Body, NewEnv),
    case OtherBindings of
	[] -> NewBody;
	_ ->
	    #let_exp{bindings=OtherBindings,
		     body=NewBody}
    end.
