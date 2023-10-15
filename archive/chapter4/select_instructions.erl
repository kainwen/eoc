-module(select_instructions).

-include("common.hrl").

-compile(export_all).

-spec select_instructions(c1_program()) -> x86_star_program().
select_instructions({c1_program,
                     {Vars, {type, Tp}},
                     C1_statements}) ->
    C1_sts = [replace_bool_in_statement(C1_st) || C1_st <- C1_statements],
    Insts = lists:flatten([transform_statement(St)|| St <- C1_sts]),
    {x86_star_program,
    Vars,
    {type, Tp},
    Insts}.

-spec transform_statement(c1_statement()) -> [x86_star_instruction()].
transform_statement(St={assign, _, _}) ->
    transform_assign(St);
transform_statement(St={return, _}) ->
    transform_return(St);
transform_statement(St={c1_if, _, _, _}) ->
    transform_if(St).

%% transform_assign for c1_arg, {read}
transform_assign({assign, V, {int, N}}) ->
    [{movq, {int, N}, V}];
transform_assign({assign, V, V1}) when is_atom(V1) ->
    [{movq, V1, V}];
transform_assign({assign, V, {read}}) ->
    [{callq, read_int},
     {movq, {register, rax}, V}];
transform_assign({assign, V, {minus, C1_arg}}) when V /= C1_arg ->
    [{negq, C1_arg},
     {movq, C1_arg, V}];
transform_assign({assign, V, {minus, C1_arg}}) when V =:= C1_arg ->
    [{negq, C1_arg}];
transform_assign({assign, V, {plus, Arg1, Arg2}}) when V =:= Arg1 ->
    [{addq, Arg2, V}];
transform_assign({assign, V, {plus, Arg1, Arg2}}) when V =:= Arg2 ->
    [{addq, Arg1, V}];
transform_assign({assign, V, {plus, Arg1, Arg2}}) ->
    [{movq, Arg1, V},
     {addq, Arg2, V}];
transform_assign({assign, V, {not_op, C1_arg}}) ->
    [{movq, C1_arg, V},
     {xorq, {int, 1}, C1_arg}];
transform_assign({assign, V, {{cmp, Cmp}, Arg1, Arg2}}) ->
    [{cmpq, Arg2, Arg1},
     {set,
      get_cond_code_from_cmp(Cmp),
      {byte_reg, {register, al}}},
     {movzbq, {byte_reg, {register, al}}, V}].

transform_return({return, Arg}) ->
    [{movq, Arg, {register, rax}}].

transform_if({c1_if, Cmp_exp, Sts1, Sts2}) ->
    {c1_if,
     Cmp_exp,
     lists:flatten([transform_statement(St) || St <- Sts1]),
     lists:flatten([transform_statement(St) || St <- Sts2])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_cond_code_from_cmp(Cmp) ->
    Dict = [
            {'eq?', e},
            {'>', g},
            {'<', l},
            {'>=', ge},
            {'=<', le}
           ],
    proplists:get_value(Cmp, Dict).

replace_bool_in_statement({assign, V, Exp}) ->
    {assign, V, replace_bool_in_exp(Exp)};
replace_bool_in_statement({return, Arg}) ->
    {return, replace_bool_in_arg(Arg)};
replace_bool_in_statement({c1_if, Cmp_exp, Sts1, Sts2}) ->
    {c1_if,
     replace_bool_in_exp(Cmp_exp),
     [replace_bool_in_statement(St) || St <- Sts1],
     [replace_bool_in_statement(St) || St <- Sts2]}.

replace_bool_in_exp({read}) -> {read};
replace_bool_in_exp({minus, Arg}) ->
    {minus, replace_bool_in_arg(Arg)};
replace_bool_in_exp({plus, Arg1, Arg2}) ->
    {plus, replace_bool_in_arg(Arg1), replace_bool_in_arg(Arg2)};
replace_bool_in_exp({not_op, Arg}) ->
    {not_op, replace_bool_in_arg(Arg)};
replace_bool_in_exp({{cmp, Cmp}, Arg1, Arg2}) ->
    {{cmp, Cmp}, replace_bool_in_arg(Arg1), replace_bool_in_arg(Arg2)};
replace_bool_in_exp(Arg) -> replace_bool_in_arg(Arg).

replace_bool_in_arg({int, N}) -> {int, N};
replace_bool_in_arg(V) when is_atom(V) -> V;
replace_bool_in_arg({bool, c1_true}) -> {int, 1};
replace_bool_in_arg({bool, c1_false}) -> {int, 0}.
