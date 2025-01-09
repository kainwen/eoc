-module(rco).

-export([remove_complex_operands/1]).

-include("ast.hrl").

-type cnt() :: counter:counter().

-spec remove_complex_operands(program()) -> mon_program().
remove_complex_operands(#program{body=Exp}) ->
    Cnt = counter:new_counter(1),
    MP = #mon_program{body=rco(Exp, Cnt)},
    ok = counter:stop_counter(Cnt),
    MP.

-spec rco(exp(), cnt()) -> mon_exp().
rco(I=#integer_const_exp{}, _Cnt) -> I;
rco(V=#var_exp{}, _Cnt) -> V;
rco(R=#read_exp{}, _Cnt) -> R;
rco(#negative_exp{body=Exp}, Cnt) ->
    case is_atom_exp(Exp) of
	true ->
	    #mon_negative_exp{body=Exp};
	false ->
	    MonExp = rco(Exp, Cnt),
	    NewVarName = make_name(Cnt),
	    Bindings = [{NewVarName, MonExp}],
	    Body = #mon_negative_exp{body=#var_exp{name=NewVarName}},
	    #mon_let_exp{bindings=Bindings, body=Body}
    end;
rco(#plus_exp{left=Left, right=Right}, Cnt) ->
    IsLeftAtom = is_atom_exp(Left),
    IsRightAtom = is_atom_exp(Right),
    case {IsLeftAtom, IsRightAtom} of
	{true, true} ->
	    #mon_plus_exp{left=Left, right=Right};
	{true, false} ->
	    MonExp = rco(Right, Cnt),
	    NewVarName = make_name(Cnt),
	    Bindings = [{NewVarName, MonExp}],
	    Body = #mon_plus_exp{left=Left,
			     right=#var_exp{name=NewVarName}},
	    #mon_let_exp{bindings=Bindings, body=Body};
	{false, true} ->
	    MonExp = rco(Left, Cnt),
	    NewVarName = make_name(Cnt),
	    Bindings = [{NewVarName, MonExp}],
	    Body = #mon_plus_exp{left=#var_exp{name=NewVarName},
			     right=Right},
	    #mon_let_exp{bindings=Bindings, body=Body};	    
	{false, false} ->
	    MonLeft = rco(Left, Cnt),
	    MonRight = rco(Right, Cnt),
	    NewVarNameLeft = make_name(Cnt),
	    NewVarNameRight = make_name(Cnt),
	    Bindings = [{NewVarNameLeft, MonLeft},
			{NewVarNameRight, MonRight}],
	    Body = #mon_plus_exp{left=#var_exp{name=NewVarNameLeft},
			     right=#var_exp{name=NewVarNameRight}},
	    #mon_let_exp{bindings=Bindings, body=Body}
    end;
rco(#sub_exp{left=Left, right=Right}, Cnt) ->
    IsLeftAtom = is_atom_exp(Left),
    IsRightAtom = is_atom_exp(Right),
    case {IsLeftAtom, IsRightAtom} of
	{true, true} ->
	    #mon_sub_exp{left=Left, right=Right};
	{true, false} ->
	    MonExp = rco(Right, Cnt),
	    NewVarName = make_name(Cnt),
	    Bindings = [{NewVarName, MonExp}],
	    Body = #mon_sub_exp{left=Left,
				right=#var_exp{name=NewVarName}},
	    #mon_let_exp{bindings=Bindings, body=Body};
	{false, true} ->
	    MonExp = rco(Left, Cnt),
	    NewVarName = make_name(Cnt),
	    Bindings = [{NewVarName, MonExp}],
	    Body = #mon_sub_exp{left=#var_exp{name=NewVarName},
				right=Right},
	    #mon_let_exp{bindings=Bindings, body=Body};	    
	{false, false} ->
	    MonLeft = rco(Left, Cnt),
	    MonRight = rco(Right, Cnt),
	    NewVarNameLeft = make_name(Cnt),
	    NewVarNameRight = make_name(Cnt),
	    Bindings = [{NewVarNameLeft, MonLeft},
			{NewVarNameRight, MonRight}],
	    Body = #mon_sub_exp{left=#var_exp{name=NewVarNameLeft},
				right=#var_exp{name=NewVarNameRight}},
	    #mon_let_exp{bindings=Bindings, body=Body}
    end;
rco(#let_exp{bindings=Bindings, body=Body}, Cnt) ->
    NewBindings = [{Var, rco(Exp, Cnt)}
		   || {Var, Exp} <- Bindings],
    #mon_let_exp{bindings=NewBindings,
		 body=rco(Body, Cnt)}.

-spec make_name(cnt()) -> atom().
make_name(Cnt) ->
    I = counter:inc_counter(Cnt),
    list_to_atom(string:join(["tmp", integer_to_list(I)], ".")).

-spec is_atom_exp(exp()) -> boolean().
is_atom_exp(#integer_const_exp{}) -> true;
is_atom_exp(#var_exp{}) -> true;
is_atom_exp(_) -> false.
     
