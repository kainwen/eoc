-module(explicate_control).

-include("types.hrl").

-export([explicate_control/1]).

-spec explicate_control(program()) -> cvar_prog().
explicate_control(#program{body=Body}) ->
    Tail = ec_tail(Body),
    StartLabel = {label, start},
    #cvar_prog{body=[{StartLabel, Tail}]}.

-spec ec_tail(exp()) -> cvar_tail().
ec_tail(#integer_const_exp{value=I}) ->
    #cvar_return{exp=#cvar_int{value=I}};
ec_tail(#read_exp{}) ->
    #cvar_return{exp=#cvar_read{}};
ec_tail(#negative_exp{body=Exp}) ->
    #cvar_return{exp=#cvar_neg{arg=to_cvar_atom(Exp)}};
ec_tail(#plus_exp{left=Left, right=Right}) ->
    #cvar_return{exp=#cvar_plus{left=to_cvar_atom(Left),
				right=to_cvar_atom(Right)}};
ec_tail(#sub_exp{left=Left, right=Right}) ->
    #cvar_return{exp=#cvar_sub{left=to_cvar_atom(Left),
			       right=to_cvar_atom(Right)}};
ec_tail(#var_exp{name=Var}) ->
    #cvar_return{exp=#cvar_var{var=Var}};
ec_tail(#let_exp{bindings=Bindings, body=Body}) ->
    Stmts = lists:flatmap(fun ({Var, Exp}) ->
				  case ec_tail(Exp) of
				      #cvar_return{exp=E} ->
					  [#cvar_assign{var=Var,
							exp=E}];
				      #cvar_seq{stmts=S,
						tail=#cvar_return{exp=E}} ->
					  S ++ [#cvar_assign{var=Var, exp=E}]
				  end
			  end, Bindings),
    Tail = ec_tail(Body),
    case Tail of
	#cvar_return{} ->
	    #cvar_seq{stmts=Stmts, tail=Tail};
	#cvar_seq{stmts=Ss, tail=T} ->
	    #cvar_seq{stmts=Stmts++Ss, tail=T}
    end.

-spec to_cvar_atom(#var_exp{}|#integer_const_exp{}) -> cvar_atm().
to_cvar_atom(#integer_const_exp{value=I}) ->
    #cvar_int{value=I};
to_cvar_atom(#var_exp{name=Var}) ->
    #cvar_var{var=Var};
to_cvar_atom(E) ->
    erlang:error({"unexpected input expr", E}).
