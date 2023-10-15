-module(assign_homes).

-include("type_spec.hrl").

-compile(export_all).

-define(DATA_SIZE, 8).

assign_homes({x86_star_program, Var_type_list, {type, _Type}, Insts}) ->
    Vars = [V || {V, T} <- Var_type_list, is_atom(T)],
    Vars_off = lists:zip(Vars, lists:seq(1, length(Vars))),
    Pointers = [V || {V, T} <- Var_type_list, is_tuple(T)],
    Pointers_off = lists:zip(Pointers, lists:seq(1, length(Pointers))),
    Code = [assign_inst(Inst, Vars_off, Pointers_off) || Inst <- Insts],
    {x86_gamma_program,
     {frame_size, ?DATA_SIZE * length(Vars)},
     {root_size, ?DATA_SIZE * length(Pointers)},
     Code}.

assign_inst(I={callq, _}) = I;
assign_inst(I={})
