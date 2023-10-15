-module(c0).

-export([eval/2]).

eval({c0_program, Vars, Sts}, Input) ->
    D = dict:from_list([{Var, undefined} || Var <- Vars]),
    {ok, IO_dev} = file:open(Input, [read]),
    eval_statements(Sts, D, IO_dev).

eval_statements([{return, Arg}|_], D, _IO_dev) ->
    eval_arg(Arg, D);
eval_statements([{assign, V, Exp}|Sts], D, IO_dev) ->
    Val = eval_exp(Exp, D, IO_dev),
    New_dict = dict:update(V, fun (_) -> Val end, D),
    eval_statements(Sts, New_dict, IO_dev).

eval_arg({int, N}, _D) -> N;
eval_arg(V, D) when is_atom(V) -> dict:fetch(V, D).

eval_exp({read}, _D, IO_dev) ->
    {ok, [N]} = io:fread(IO_dev, '', "~d"),
    N;
eval_exp({minus, Exp}, D, IO_dev) ->
    -eval_exp(Exp, D, IO_dev);
eval_exp({plus, Exp1, Exp2}, D, IO_dev) ->
    eval_exp(Exp1, D, IO_dev) + eval_exp(Exp2, D, IO_dev);
eval_exp(Arg, D, _) -> eval_arg(Arg, D).
