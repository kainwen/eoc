-module(patch_instructions).

-include("common.hrl").

-export([patch_instructions/1]).

-spec patch_instructions(x86_64_program()) -> x86_64_program().
patch_instructions({program, N, Insts}) ->
    {program,
     N,
     lists:flatten([patch_instruction(Inst) || Inst <- Insts])}.

-spec patch_instruction(instruction()) -> [instruction()].
patch_instruction({movq, A, A}) -> [];
patch_instruction({Inst, {deref, N1, R1}, {deref, N2, R2}}) ->
    [{movq, {deref, N1, R1}, {register, rax}},
     {Inst, {register, rax}, {deref, N2, R2}}];
patch_instruction(Inst) -> [Inst].
