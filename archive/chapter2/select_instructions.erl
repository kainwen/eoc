-module(select_instructions).

-include("common.hrl").

-compile(export_all).

-spec select_instructions(c0_program()) -> x86_64_star_program().
select_instructions({c0_program, C0_vars, C0_statements}) ->
    {x86_64_star_program,
     C0_vars,
     lists:flatten([transform_statement(St) || St <- C0_statements])}.

-spec transform_statement(c0_statement()) -> [x86_64_star_inst()].
transform_statement({assign, C0_var, {int, N}}) ->
    [{movq, {int, N}, C0_var}];
transform_statement({assign, C0_var, Var}) when is_atom(Var) ->
    [{movq, C0_var, Var}];
transform_statement({assign, C0_var, {read}}) ->
    [{callq, read_int},
     {movq, {register, rax}, C0_var}];
transform_statement({assign, C0_var, {minus, Var}}) when C0_var /= Var ->
    [{negq, Var},
     {movq, Var, C0_var}];
transform_statement({assign, C0_var, {minus, Var}}) when C0_var =:= Var ->
    [{negq, C0_var}];
transform_statement({assign, C0_var, {plus, C0_var, Arg2}}) ->
    [{addq, Arg2, C0_var}];
transform_statement({assign, C0_var, {plus, Arg1, C0_var}}) ->
    [{addq, Arg1, C0_var}];
transform_statement({assign, C0_var, {plus, Arg1, Arg2}}) ->
    [{movq, Arg1, C0_var},
     {addq, Arg2, C0_var}];
transform_statement({return, Arg}) ->
    [{movq, Arg, {register, rax}}].
