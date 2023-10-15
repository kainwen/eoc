-module(assign_homes).

-include("common.hrl").

-export([assign_homes/1]).

-define(INT_SIZE, 8).

-spec assign_homes(x86_64_mu_program()) -> x86_64_program().
assign_homes({x86_64_mu_program, Var_reg_map, Insts}) ->
    Lvars = length(Var_reg_map),
    Frame_size = case Lvars rem 2 of
                     1 -> (Lvars + 1) * ?INT_SIZE;
                     0 -> Lvars * ?INT_SIZE
                 end,
    {program,
     Frame_size,
     [assign_inst_home(Inst, Var_reg_map) || Inst <- Insts]}.

assign_inst_home(Inst={callq, _}, _Map) -> Inst;
assign_inst_home({Inst_name}, _Map) -> {Inst_name};
assign_inst_home({Inst, Arg}, Map) ->
    {Inst, assign_arg_home(Arg, Map)};
assign_inst_home({Inst, Arg1, Arg2}, Map) ->
    {Inst, assign_arg_home(Arg1, Map), assign_arg_home(Arg2, Map)}.

assign_arg_home(V, Map) when is_atom(V) ->
    case proplists:get_value(V, Map) of
        undefined -> erlang:error(can_not_find_var_in_maps);
        N when is_integer(N) -> {deref, N, rbp};
        Reg -> Reg
    end;
assign_arg_home(Arg, _Map) -> Arg.
