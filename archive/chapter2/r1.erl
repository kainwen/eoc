-module(r1).

-include("common.hrl").

-export([eval/2, eval_exp/3, eval_script/2]).

-spec eval(r1_program(), pid()) -> integer().
eval({r1_program, Exp}, F) ->
    eval_exp(Exp, env:empty_env(), F).

-spec eval_exp(r1_exp(), env:env(), pid()) -> integer().
eval_exp({int, N}, _Env, _F) -> N;
eval_exp({read}, _Env, F) ->
    {ok, [N]} = io:fread(F, '', "~d"),
    N;
eval_exp({minus, E}, Env, F) ->
    -eval_exp(E, Env, F);
eval_exp({plus, E1, E2}, Env, F) ->
    eval_exp(E1, Env, F) + eval_exp(E2, Env, F);
eval_exp({var, Var}, Env, _F) ->
    env:apply_env(Env, Var);
eval_exp({'let', Vars, Exps, Body}, Env, F) ->
    Vals = [eval_exp(Exp, Env, F) || Exp <- Exps],
    New_env = env:extend_env(Env, lists:zip(Vars, Vals)),
    eval_exp(Body, New_env, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
-spec eval_script(string(), string()) -> integer().
eval_script(Fn, Input) ->
    Program = r1_parse:parse_file(Fn),
    {ok, F} = file:open(Input, [read]),
    eval(Program, F).
