-module(r2).

-export([eval_script/1]).

eval_exp({int, N}, _Env) -> N;
eval_exp({read}, _Env) ->
    {ok, [N]} = io:fread('', "~d"),
    N;
eval_exp({minus, Exp}, Env) ->
    -eval_exp(Exp, Env);
eval_exp({plus, Exp1, Exp2}, Env) ->
    eval_exp(Exp1, Env) + eval_exp(Exp2, Env);
eval_exp({var, V}, Env) ->
    case proplists:get_value(V, Env) of
        undefined ->
            erlang:error({can_not_find_var, V});
        N -> N
    end;
eval_exp({'let', Vars, Exps, Body}, Env) ->
    Vals = [eval_exp(Exp, Env) || Exp <- Exps],
    New_env = lists:zip(Vars, Vals) ++ Env,
    eval_exp(Body, New_env);
eval_exp({true_exp}, _) -> true;
eval_exp({false_exp}, _) -> false;
eval_exp({and_exp, Exp1, Exp2}, Env) ->
    case eval_exp(Exp1, Env) of
        false -> false;
        true ->
            eval_exp(Exp2, Env)
    end;
eval_exp({not_exp, Exp}, Env) ->
    not eval_exp(Exp, Env);
eval_exp({{cmp, Cmp}, Exp1, Exp2}, Env) ->
    V1 = eval_exp(Exp1, Env),
    V2 = eval_exp(Exp2, Env),
    case Cmp of
        'eq?' -> V1 =:= V2;
        '<=' -> V1 =< V2;
        '>=' -> V1 >= V2;
        '<' -> V1 < V2;
        '>' -> V1 > V2
    end;
eval_exp({if_exp, Exp1, Exp2, Exp3}, Env) ->
    case eval_exp(Exp1, Env) of
        true ->
            eval_exp(Exp2, Env);
        false ->
            eval_exp(Exp3, Env)
    end.

eval_script(Fn) ->
    {r2_program, {type, _Tp}, Exp} = r2_parse:parse_file(Fn),
    eval_exp(Exp, []).
