-module(flatten).

-include("common.hrl").

-export([flatten/1, flatten_test/0]).


-spec flatten(r1_program()) -> c0_program().
flatten({r1_program, R1_exp}) ->
    var_server:start(),
    {Sts, {return, V}} = flatten_exp(R1_exp),
    {c0_program, var_server:get_var_list(), Sts ++ [{return, V}]}.

-spec flatten_exp(r1_exp()) -> {[c0_statement()], {return, c0_arg()}}.
flatten_exp({int, N}) ->
    {[], {return, {int, N}}};
flatten_exp({read}) ->
    Var = var_server:new_var(),
    Sts = [{assign, Var, {read}}],
    {Sts, {return, Var}};
flatten_exp({minus, {int, N}}) ->
    {[], {return, {int, -N}}};
flatten_exp({minus, R1_exp}) ->
    Var = var_server:new_var(),
    {Sts, {return, C0_arg}} = flatten_exp(R1_exp),
    Final_sts = Sts ++ [{assign, Var, {minus, C0_arg}}],
    {Final_sts, {return, Var}};
flatten_exp({plus, {int, N1}, {int, N2}}) ->
    {[], {return, {int, N1+N2}}};
flatten_exp({plus, R1_exp1, R1_exp2}) ->
    Var = var_server:new_var(),
    {Sts1, {return, C0_arg1}} = flatten_exp(R1_exp1),
    {Sts2, {return, C0_arg2}} = flatten_exp(R1_exp2),
    Final_sts = Sts1 ++ Sts2 ++ [{assign, Var, {plus, C0_arg1, C0_arg2}}],
    {Final_sts, {return, Var}};
flatten_exp({var, V}) ->
    {[], {return, V}};
flatten_exp({'let', Vars, Exps, Body}) ->
    Results = lists:map(fun make_bind/1, lists:zip(Vars, Exps)),
    var_server:add_vars(Vars),
    {Sts_body, {return, Arg_body}} = flatten_exp(Body),
    Final_sts = lists:flatten([Sts || {Sts, _} <- Results]) ++ Sts_body,
    {Final_sts, {return, Arg_body}}.

make_bind({Var, Exp}) ->
    {Sts, {return, C_arg}} = flatten_exp(Exp),
    case is_atom(C_arg) of
        true ->
            ok = var_server:delete_var(C_arg),
            {[rep_var_in_c0_st(St, C_arg, Var) || St <- Sts], {return, Var}};
        false ->
            {Sts ++ [{assign, Var, C_arg}], {return, Var}}
    end.

rep_var_in_c0_st({assign, V, C0_exp}, V_target, V_want) when V =:= V_target ->
    {assign, V_want, C0_exp};
rep_var_in_c0_st({assign, V, C0_exp}, V_target, V_want) when V /= V_target ->
    {assign, V, rep_var_in_c0_exp(C0_exp, V_target, V_want)}.

rep_var_in_c0_exp({read}, _, _) -> {read};
rep_var_in_c0_exp({minus, Arg1, Arg2}, V_target, V_want) ->
    {minus,
     rep_var_in_c0_arg(Arg1, V_target, V_want),
     rep_var_in_c0_arg(Arg2, V_target, V_want)};
rep_var_in_c0_exp({plus, Arg1, Arg2}, V_target, V_want) ->
    {plus,
     rep_var_in_c0_arg(Arg1, V_target, V_want),
     rep_var_in_c0_arg(Arg2, V_target, V_want)};
rep_var_in_c0_exp(Arg, V_target, V_want) ->
    rep_var_in_c0_arg(Arg, V_target, V_want).

rep_var_in_c0_arg({int, N}, _, _) -> {int, N};
rep_var_in_c0_arg(V, V_target, V_want) when V =:= V_target -> V_want;
rep_var_in_c0_arg(V, V_target, _V_want) when V /= V_target -> V.

flatten_test() ->
    R1_code_dir = "test/code",
    Input_dir = "test/input",
    Output_dir = "test/out1",
    {ok, Fns} = file:list_dir(R1_code_dir),
    [check(Fn, R1_code_dir, Input_dir, Output_dir) || Fn <- Fns].

check(Fn, R1_code_dir, Input_dir, Output_dir) ->
    Code_path = filename:join(R1_code_dir, Fn),
    Input_path = filename:join(Input_dir, string:join([Fn, "in"], ".")),
    Ans1 = r1:eval_script(Code_path, Input_path),

    R1_ast = r1_parse:parse_file(Code_path),
    C0_ast = flatten(uniquify:uniquify(R1_ast)),
    Ans2 = c0:eval(C0_ast, Input_path),

    Pos = string:str(Fn, ".ss"),
    Std_ans_path = filename:join(Output_dir, string:sub_string(Fn, 1, Pos-1)),
    {ok, Std_ans} = file:open(Std_ans_path, [read]),
    {ok, [Ans3]} = io:fread(Std_ans, '', "~d"),

    case Ans1 =:= Ans2 of
        true ->
            Ans1 =:= Ans3;
        false ->
            false
    end.
