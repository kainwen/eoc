-module(partial_eval).

-include("types.hrl").

-export([pe/1, pe_prog/1]).

-spec pe_prog(program()) -> program().
pe_prog(#program{body=Exp}) ->
    #program{body=pe(Exp)}.

-spec pe(exp()) -> exp().
pe(Exp=#integer_const_exp{}) -> Exp;
pe(Exp=#read_exp{}) -> Exp;
pe(_Exp=#negative_exp{body=Body}) ->
    case pe(Body) of
	#integer_const_exp{value=N} ->
	    #integer_const_exp{value=-N};
	NewBody ->
	    #negative_exp{body=NewBody}
    end;
pe(_Exp=#plus_exp{left=Left, right=Right}) ->
    binop_pe(Left, Right, '+');
pe(_Exp=#sub_exp{left=Left, right=Right}) ->
    binop_pe(Left, Right, '-');
pe(Exp=#var_exp{}) -> Exp;
pe(_Exp=#let_exp{bindings=Bindings, body=Body}) ->
    NewBody = pe(Body),
    case NewBody of
	#integer_const_exp{} ->
	    NewBody;
	_ ->
	    NewBindings = [{Name, pe(BExp)} || {Name, BExp} <- Bindings],
	    #let_exp{bindings=NewBindings, body=NewBody}
    end.

%% internal helper function
-spec binop_pe(exp(), exp(), binop()) -> exp().
binop_pe(Left, Right, BinOp) ->
    NewLeft = pe(Left),
    NewRight = pe(Right),
    case {NewLeft, NewRight} of
	{#integer_const_exp{value=N}, #integer_const_exp{value=M}} ->
	case BinOp of
	    '+' ->
		#integer_const_exp{value=N+M};
	    '-' ->
		#integer_const_exp{value=N-M}
	end;
	_ ->
	    case BinOp of
		'+' ->
		    #plus_exp{left=NewLeft, right=NewRight};
		'-' ->
		    #sub_exp{left=NewLeft, right=NewRight}
	    end
    end.
