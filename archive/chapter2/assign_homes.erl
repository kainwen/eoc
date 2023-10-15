-module(assign_homes).

-include("common.hrl").

-export([assign_homes/2, assign_homes/1]).

-define(INT_SIZE, 8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exercise 6. Implement the assign-homes pass and test it on all of the ex-
%% ample programs that you created for the previous passes pass. I recommend
%% that assign-homes take an extra parameter that is a mapping of variable
%% names to homes (stack locations for now). Use the interp-tests function
%% (Appendix 12.2) from utilities.rkt to test your passes on the example
%% programs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type maps() :: [{x86_64_star_var(), integer()}].

-spec assign_homes(x86_64_star_program()) -> x86_64_program().
assign_homes(Prog={x86_64_star_program, Vars, _Insts}) ->
    Indexes = lists:seq(1, length(Vars)),
    Maps = [{V, -I * ?INT_SIZE} || {I, V} <- lists:zip(Indexes, Vars)],
    assign_homes(Prog, Maps).

-spec assign_homes(x86_64_star_program(), maps()) -> x86_64_program().
assign_homes({x86_64_star_program, Vars, Insts}, Var_pos_map) ->
    Lvars = length(Vars),
    Frame_size = case Lvars rem 2 of
                     1 -> (Lvars + 1) * ?INT_SIZE;
                     0 -> Lvars * ?INT_SIZE
                 end,
    {program,
     Frame_size,
     [assign_inst_home(Inst, Var_pos_map) || Inst <- Insts]}.

-spec assign_inst_home(x86_64_star_inst(), maps()) -> instruction().
assign_inst_home(Inst={callq, _}, _Map) -> Inst;
assign_inst_home({Inst_name}, _Map) -> {Inst_name};
assign_inst_home({Inst, Arg}, Map) ->
    {Inst, assign_arg_home(Arg, Map)};
assign_inst_home({Inst, Arg1, Arg2}, Map) ->
    {Inst, assign_arg_home(Arg1, Map), assign_arg_home(Arg2, Map)}.

-spec assign_arg_home(x86_64_arg(), maps()) -> x86_64_arg().
assign_arg_home(V, Map) when is_atom(V) ->
    Off_set = case proplists:get_value(V, Map) of
                  undefined -> erlang:error(can_not_find_var_in_maps);
                  N -> N
              end,
    {deref, Off_set, rbp};
assign_arg_home(Arg, _Map) -> Arg.
