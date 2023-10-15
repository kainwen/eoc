-module(r0).

-include("common.hrl").

-export([eval/2, eval_exp/2, eval_script/2]).

-spec eval(program(), pid()) -> integer().
eval({program, Exp}, IO_dev) ->
    eval_exp(Exp, IO_dev).

-spec eval_exp(exp(), pid()) -> integer().
eval_exp({int, N}, _IO_dev) -> N;
eval_exp({read}, IO_dev) ->
    {ok, [N]} = io:fread(IO_dev, '', "~d"),
    N;
eval_exp({minus, E}, IO_dev) ->
    -eval_exp(E, IO_dev);
eval_exp({plus, E1, E2}, IO_dev) ->
    eval_exp(E1, IO_dev) + eval_exp(E2, IO_dev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
-spec eval_script(string(), string()) -> integer().
eval_script(Fn, Input) ->
    Program = r0_parse:parse_file(Fn),
    {ok, IO_dev} = file:open(Input, [read]),
    eval(Program, IO_dev).
