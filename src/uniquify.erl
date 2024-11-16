-module(uniquify).

-export([uniquify/1]).

-include("ast.hrl").

-type value() :: atom().
-type env() :: env:env() | env:env(value()).
-type cnt() :: counter:counter().

-spec uniquify(program()) -> program().
uniquify(#program{body=Exp}) ->
    Env=  env:new(),
    Cnt = counter:new_counter(1),
    Prog = #program{body=uniquify_exp(Exp, Env, Cnt)},
    ok = counter:stop_counter(Cnt),
    Prog.

-spec uniquify_exp(exp(), env(), cnt()) -> exp().
uniquify_exp(I=#integer_const_exp{}, _Env, _Cnt) -> I;
uniquify_exp(R=#read_exp{}, _Env, _Cnt) -> R;
uniquify_exp(#negative_exp{body=Exp}, Env, Cnt) ->
    #negative_exp{body=uniquify_exp(Exp, Env, Cnt)};
uniquify_exp(#plus_exp{left=L, right=R}, Env, Cnt) ->
    #plus_exp{left=uniquify_exp(L, Env, Cnt),
	      right=uniquify_exp(R, Env, Cnt)};
uniquify_exp(#sub_exp{left=L, right=R}, Env, Cnt) ->
    #sub_exp{left=uniquify_exp(L, Env, Cnt),
	     right=uniquify_exp(R, Env, Cnt)};
uniquify_exp(#var_exp{name=Name}, Env, _Cnt) ->
    case env:find(Name, Env) of
	fail ->
	    erlang:error({"unknown var", Name});
	{ok, NewName} ->
	    #var_exp{name=NewName}
    end;
uniquify_exp(#let_exp{bindings=Bindings, body=Body}, Env, Cnt) ->
    NewExps = [uniquify_exp(Exp, Env, Cnt) || {_, Exp} <- Bindings],
    Names = [Name || {Name, _} <- Bindings],
    I = counter:inc_counter(Cnt),
    NewNames = [make_name(Name, I) || {Name, _} <- Bindings],
    NewEnv = env:extend(lists:zip(Names, NewNames), Env),
    #let_exp{bindings=lists:zip(NewNames, NewExps),
	     body=uniquify_exp(Body, NewEnv, Cnt)}.

%% internal helper
-spec make_name(atom(), integer()) -> atom().
make_name(Name, I) ->
    S = string:join([atom_to_list(Name), integer_to_list(I)], "."),
    list_to_atom(S).

