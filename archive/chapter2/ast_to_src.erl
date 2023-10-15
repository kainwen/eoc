-module(ast_to_src).

-include("common.hrl").

-export([to_r1/1]).

-spec to_r1(r1_program()) -> string().
to_r1({r1_program, Exp}) ->
    string:join(["(program ", to_r1_exp(Exp), ")"], "").

-spec to_r1_exp(r1_exp()) -> string().
to_r1_exp({int, N}) -> integer_to_list(N);
to_r1_exp({read}) -> "(read)";
to_r1_exp({minus, E}) ->
    Estr = to_r1_exp(E),
    string:join(["(- ", Estr, ")"], "");
to_r1_exp({plus, E1, E2}) ->
    Estr1 = to_r1_exp(E1),
    Estr2 = to_r1_exp(E2),
    string:join(["(+ ", Estr1, " ", Estr2, ")"], "");
to_r1_exp({var, V}) -> atom_to_list(V);
to_r1_exp({'let', Vars, Exps, Body}) ->
    Bindings = [string:join(["[", atom_to_list(Var), " ", to_r1_exp(Exp), "]"], "")
                || {Var, Exp} <- lists:zip(Vars, Exps)],
    string:join(["(let (",
                 string:join(Bindings, " "),
                 ")", " ", to_r1_exp(Body), ")"], "").
