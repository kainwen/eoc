-module(lower_condition).

-include("common.hrl").

-export([lower_condition/1]).

-spec lower_condition(x86_star_program()) -> x86_1_program().
lower_condition({x86_star_program, Info, {type, Tp}, Insts}) ->
    gensym:start(),
    {x86_1_program,
     Info,
     {type, Tp},
     lists:flatten([lower_if(Inst) || Inst <- Insts])}.

lower_if({c1_if, {{cmp, Cmp}, Arg1, Arg2}, Insts1, Insts2}) ->
    Then_label = {label, gensym:new_sym('then')},
    If_end_label = {label, gensym:new_sym('if_end')},
    lists:flatten([{cmpq, Arg1, Arg2},
                   {jmp_if, get_cond_code_from_cmp(Cmp), Then_label},
                   [lower_if(Inst) || Inst <- Insts2],
                   {jmp, If_end_label},
                   Then_label,
                   [lower_if(Inst) || Inst <- Insts1],
                   If_end_label]);
lower_if(Inst) -> Inst.

get_cond_code_from_cmp(Cmp) ->
    Dict = [
            {'eq?', e},
            {'>', g},
            {'<', l},
            {'>=', ge},
            {'=<', le}
           ],
    proplists:get_value(Cmp, Dict).
