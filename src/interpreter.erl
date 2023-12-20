-module(interpreter).

-include("types.hrl").

-export([interp/1]).

-spec interp(program()) -> value().
interp(Prog) ->
    BodyExp = Prog#program.body,
    interp_exp(BodyExp, env:empty()).

-spec interp_exp(exp(), env()) -> value().
interp_exp(Exp=#integer_const_exp{}, Env) ->
    interp_integer_const_exp(Exp, Env);
interp_exp(Exp=#read_exp{}, Env) ->
    interp_read_exp(Exp, Env);
interp_exp(Exp=#negative_exp{}, Env) ->
    interp_negative_exp(Exp, Env);
interp_exp(Exp=#plus_exp{}, Env) ->
    interp_plus_exp(Exp, Env);
interp_exp(Exp=#sub_exp{}, Env) ->
    interp_sub_exp(Exp, Env);
interp_exp(Exp=#var_exp{}, Env) ->
    interp_var_exp(Exp, Env);
interp_exp(Exp=#let_exp{}, Env) ->
    interp_let_exp(Exp, Env);
interp_exp(_Exp, _) ->
    erlang:error({"unexpected type of expression", _Exp}).

-spec interp_integer_const_exp(#integer_const_exp{}, env()) -> value().
interp_integer_const_exp(#integer_const_exp{value=N}, _) ->
    #int_value{value=N}.

-spec interp_read_exp(#read_exp{}, env()) -> value().
interp_read_exp(_ReadExp, _Env) ->
    N = utils:read_from_stdin(),
    #int_value{value=N}.

-spec interp_negative_exp(#negative_exp{}, env()) -> value().
interp_negative_exp(#negative_exp{body=Body}, Env) ->
    Val = interp_exp(Body, Env),
    case Val of
	#int_value{value=N} ->
	    #int_value{value=-N};
	_ ->
	    erlang:error({"negative exp's body should eval to int value", Body, Val})
    end.

-spec interp_plus_exp(#plus_exp{}, env()) -> value().
interp_plus_exp(#plus_exp{left=Left, right=Right}, Env) ->
    interp_binop_exp(Left, Right, Env, '+').

-spec interp_sub_exp(#sub_exp{}, env()) -> value().
interp_sub_exp(#sub_exp{left=Left, right=Right}, Env) ->
    interp_binop_exp(Left, Right, Env, '-').

-spec interp_var_exp(#var_exp{}, env()) -> value().
interp_var_exp(#var_exp{name=VarName}, Env) ->
    case env:find(VarName, Env) of
	{ok, Val} ->
	    Val;
	 false ->
	    erlang:error({"cannot find variable", VarName, Env})
    end.

-spec interp_let_exp(#let_exp{}, env()) -> value().
interp_let_exp(#let_exp{bindings=Bindings, body=Body}, Env) ->
    BindingVals = [{Name, interp_exp(Exp, Env)} || {Name, Exp} <- Bindings],
    LetBodyEnv = env:extend(BindingVals, Env),
    interp_exp(Body, LetBodyEnv).

%% internal helper functions
-spec interp_binop_exp(exp(), exp(), env(), binop()) -> value().
interp_binop_exp(Left, Right, Env, BinOp) ->
    LeftVal = interp_exp(Left, Env),
    case LeftVal of
	#int_value{value=LeftN} ->
	    RightVal = interp_exp(Right, Env),
	    case RightVal of
		#int_value{value=RightN} ->
		    NewVal = case BinOp of
				 '+' -> LeftN + RightN;			    
				 '-' -> LeftN - RightN;
				 _ -> erlang:error({"unknown binop", BinOp})
			     end,
		    #int_value{value=NewVal};
		_ ->
		    erlang:error({"want int value here", Right, RightVal})
	    end;
	_ ->
	    erlang:error({"want int value here", Left, LeftVal})
    end.
