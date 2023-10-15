-module(patch_instructions).

-include("common.hrl").

-export([patch_instructions/1]).

-spec patch_instructions(x86_1_program()) -> x86_1_program().
patch_instructions({x86_1_program, Info, {type, Tp}, Insts}) ->
    {x86_1_program,
     Info,
     {type, Tp},
     lists:flatten([patch_instruction(Inst) || Inst <- Insts])}.

patch_instruction({cmpq, Arg1, {int, N}}) ->
    [{movq, {int, N}, {register, rax}},
     {cmpq, Arg1, {register, rax}}];
patch_instruction({movq, A, A}) -> [];
patch_instruction({Inst, {deref, N1, R1}, {deref, N2, R2}}) ->
    [{movq, {deref, N1, R1}, {register, rax}},
     {Inst, {register, rax}, {deref, N2, R2}}];
patch_instruction(Inst) -> [Inst].
