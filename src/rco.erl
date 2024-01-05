-module(rco).

-include("types.hrl").

-export([remove_complex_operands/1]).

-spec remove_complex_operands(program()) -> program().
remove_complex_operands(#program{body=Exp}) ->
    Counter = counter:new(0),
    #program{body=rco(Exp, Counter)}.

-spec rco(exp(), counter()) -> exp().
rco(Exp=#integer_const_exp{}, _Cnt) -> Exp;
rco(Exp=#read_exp{}, _Cnt) -> Exp;
rco(#negative_exp{body=Body}, Cnt) ->
    NewBody = rco(Body, Cnt),
    {Bindings, _SubInfos} = handle_subparts([NewBody], Cnt),
    case Bindings of
	[] ->
	    #negative_exp{body=NewBody};
	[{TmpVar, _}] ->
	    #let_exp{bindings=Bindings,
		     body=#negative_exp{body=#var_exp{name=TmpVar}}}
    end;
rco(#plus_exp{left=Left, right=Right}, Cnt) ->
    NewLeft = rco(Left, Cnt),
    NewRight = rco(Right, Cnt),
    {Bindings, SubInfos} = handle_subparts([NewLeft, NewRight], Cnt),
    case Bindings of
	[] ->
	    #plus_exp{left=NewLeft, right=NewRight};
	_ ->
	    NewBody = #plus_exp{left=fetch_subinfo(1, SubInfos, NewLeft),
				right=fetch_subinfo(2, SubInfos, NewRight)},
	    #let_exp{bindings=Bindings, body=NewBody}
    end;
rco(#sub_exp{left=Left, right=Right}, Cnt) ->
    NewLeft = rco(Left, Cnt),
    NewRight = rco(Right, Cnt),
    {Bindings, SubInfos} = handle_subparts([NewLeft, NewRight], Cnt),
    case Bindings of
	[] ->
	    #sub_exp{left=NewLeft, right=NewRight};
	_ ->
	    NewBody = #sub_exp{left=fetch_subinfo(1, SubInfos, NewLeft),
				right=fetch_subinfo(2, SubInfos, NewRight)},
	    #let_exp{bindings=Bindings, body=NewBody}
    end;
rco(Exp=#var_exp{}, _Cnt) -> Exp;
rco(#let_exp{bindings=Bindings, body=Body}, Cnt) ->
    NewBindings = [{Var, rco(Exp, Cnt)}
		   || {Var, Exp}<- Bindings],
    NewBody = rco(Body, Cnt),
    #let_exp{bindings=NewBindings, body=NewBody}.


%% Internal helper: build bindings
-spec is_exp_atomic(exp()) -> boolean().
is_exp_atomic(_Exp=#integer_const_exp{}) -> true;
is_exp_atomic(_Exp=#var_exp{}) -> true;
is_exp_atomic(_) -> false.

-type subinfo() :: {ok, atom()} | false.
-spec handle_subparts([exp()], counter()) -> {[binding()], [subinfo()]}.
handle_subparts(Exps, Cnt) ->
    SubInfos = [case is_exp_atomic(Exp) of
		   true ->
		       false;
		   false ->
		       {ok, tmp_name(Cnt)}
		end
		|| Exp <- Exps],
    Bindings = [{element(2, SubInfo), Exp}
		|| {SubInfo, Exp} <- lists:zip(SubInfos, Exps),
		   SubInfo /= false],
    {Bindings, SubInfos}.

-spec fetch_subinfo(integer(), [subinfo()], exp()) -> exp().
fetch_subinfo(I, SubInfos, Exp) ->
    case lists:nth(I, SubInfos) of
	false ->
	    Exp;
	{ok, TmpVar} ->
	    #var_exp{name=TmpVar}
    end.
    
%% Internal helper: Get temp name
-spec tmp_name(counter()) -> atom().
tmp_name(Counter) ->
    N = counter:fetch(Counter),
    ok = counter:bump(Counter),
    list_to_atom(string:join(["tmp", integer_to_list(N)],
			     ".")).
