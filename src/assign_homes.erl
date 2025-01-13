-module(assign_homes).

-export([assign_homes/1]).

-include("ast.hrl").

-spec assign_homes(x86_var()) -> x86_int().

assign_homes(#x86_var{body=[{Label, Block}]}) ->
    NewBlock = ah(Block),
    #x86_int{body=[{Label, NewBlock}]}.

-spec ah(#x86_var_block{}) -> #x86_int_block{}.
ah(#x86_var_block{instructions=Instructions}) ->
    VarDict = get_all_vars(Instructions, dict:new()),
    #x86_int_block{instructions=[ah_instr(Instruction, VarDict)
				 || Instruction <- Instructions]}.

-type var_dict() :: dict:dict() | dict:dict(atom(), integer()).
-spec ah_instr(x86_var_instr(), var_dict()) -> x86_int_instr().
ah_instr(#x86_var_addq{dest=Dest, src=Src}, VarDict) ->
    #x86_int_addq{dest=ah_arg(Dest, VarDict),
		  src=ah_arg(Src, VarDict)};
ah_instr(#x86_var_subq{dest=Dest, src=Src}, VarDict) ->
    #x86_int_subq{dest=ah_arg(Dest, VarDict),
		  src=ah_arg(Src, VarDict)};
ah_instr(#x86_var_negq{dest=Dest}, VarDict) ->
    #x86_int_negq{dest=ah_arg(Dest, VarDict)};
ah_instr(#x86_var_movq{dest=Dest, src=Src}, VarDict) ->
    #x86_int_movq{dest=ah_arg(Dest, VarDict),
		  src=ah_arg(Src, VarDict)};
ah_instr(#x86_var_pushq{arg=Arg}, VarDict) ->
    #x86_int_pushq{arg=ah_arg(Arg, VarDict)};
ah_instr(#x86_var_popq{arg=Arg}, VarDict) ->
    #x86_int_popq{arg=ah_arg(Arg, VarDict)};
ah_instr(Instr, _) -> Instr.

-spec ah_arg(x86_var_arg(), var_dict()) -> x86_int_arg().
ah_arg(V=#var_exp{}, VarDict) ->
    Order = dict:fetch(V, VarDict),
    Reref = #deref{reg=#reg{name=rbp}, offset=-8*Order},
    Reref;
ah_arg(Arg, _) -> Arg.

-spec get_all_vars([x86_var_instr()], var_dict()) -> var_dict().
get_all_vars(Instructions, VarDict) ->
    lists:foldl(fun (Instruction, Dict) ->
			Vars = case Instruction of
				   #x86_var_addq{dest=D,src=S} ->
				       [D,S];
				   #x86_var_subq{dest=D,src=S} ->
				       [D,S];
				   #x86_var_negq{dest=D} ->
				       [D];
				   #x86_var_movq{dest=D,src=S} ->
				       [D,S];
				   #x86_var_pushq{arg=A} ->
				       [A];
				   #x86_var_popq{arg=A} ->
				       [A];
				   _ ->
				       []
			       end,
			RemVars = [Var
				   || Var <- Vars, is_record(Var, var_exp)],
			case RemVars of
			    [] -> Dict;
			    [V] ->
				case dict:is_key(V, Dict) of
				    true -> Dict;
				    false ->
					dict:store(V, dict:size(Dict) + 1, Dict)
				end;
			    [V1, V2] ->
				Dict1 = case dict:is_key(V1, Dict) of
					    true -> Dict;
					    false ->
						dict:store(V1, dict:size(Dict) + 1, Dict)
					end,
				case dict:is_key(V2, Dict1) of
				    true -> Dict1;
				    false ->
					dict:store(V2, dict:size(Dict1) + 1, Dict1)
				end
			end
		end,
		VarDict, Instructions).

