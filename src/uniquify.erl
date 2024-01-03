-module(uniquify).

-include("types.hrl").

-type uenv() :: [{integer(), [atom()]}].

-export([uniquify/1]).

%% Each variable belongs to exactly one environment,
%% for each environment, we generate a unique number,
%% then for each variable in that environment renaming
%% them by appending the number.

-spec uniquify(program()) -> program().
uniquify(#program{body=Body}) ->
    Counter = counter:new(0),
    Uenv = new_uenv(),
    NewBody = uniquify_exp(Body, Counter, Uenv),
    Prog = #program{body=NewBody},
    ok = counter:shutdown(Counter),
    Prog.

-spec uniquify_exp(exp(), pid(), uenv()) -> exp().
uniquify_exp(Exp=#integer_const_exp{}, _Cnt, _Uenv) -> Exp;
uniquify_exp(Exp=#read_exp{}, _Cnt, _Uenv) -> Exp;
uniquify_exp(#negative_exp{body=Exp}, Cnt, Uenv) ->
    #negative_exp{body=uniquify_exp(Exp, Cnt, Uenv)};
uniquify_exp(#plus_exp{left=Left, right=Right}, Cnt, Uenv) ->
    #plus_exp{left=uniquify_exp(Left, Cnt, Uenv),
	      right=uniquify_exp(Right, Cnt, Uenv)};
uniquify_exp(#sub_exp{left=Left, right=Right}, Cnt, Uenv) ->
    #sub_exp{left=uniquify_exp(Left, Cnt, Uenv),
	     right=uniquify_exp(Right, Cnt, Uenv)};
uniquify_exp(#var_exp{name=Var}, _Cnt, Uenv) ->
    case find_uenv(Uenv, Var) of
	{ok, N} ->
	    NewVar = new_varname(Var, N),
	    #var_exp{name=NewVar};
	false ->
	    erlang:error({"cannot find variable", Var})
    end;
uniquify_exp(#let_exp{bindings=Bindings, body=Body}, Cnt, Uenv) ->
    NewUenv = extend_uenv(Uenv, [Var || {Var, _} <- Bindings], Cnt),
    N = counter:fetch(Cnt),
    NewBody = uniquify_exp(Body, Cnt, NewUenv),
    NewBindings = [{new_varname(Var, N), uniquify_exp(Exp, Cnt, Uenv)}
		   || {Var, Exp} <- Bindings],
    #let_exp{bindings=NewBindings, body=NewBody}.    

%% internal helper function: uenv
-spec new_uenv() -> uenv().
new_uenv() -> [].

-spec extend_uenv(uenv(), [atom()], pid()) -> uenv().
extend_uenv(Uenv, Vars, Counter) ->
    ok = counter:bump(Counter),
    N = counter:fetch(Counter),
    [{N, Vars}|Uenv].

-spec find_uenv(uenv(), atom()) -> {ok, integer()} | false.
find_uenv([], _Var) -> false;
find_uenv([{N, Vars}|Uenv], Var) ->
    case lists:member(Var, Vars) of
	true ->
	    {ok, N};
	false ->
	    find_uenv(Uenv, Var)
    end.
    
%% internal helper
-spec new_varname(atom(), integer()) -> atom().
new_varname(Var, N) ->
    list_to_atom(string:join([atom_to_list(Var),
			      integer_to_list(N)], ".")).
