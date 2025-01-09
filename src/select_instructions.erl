-module(select_instructions).

-include("ast.hrl").

-export([select_instructions/1]).

-spec select_instructions(cvar_program()) -> x86_var().
select_instructions(#cvar_program{body=[{_Label, Tail}]}) ->
    Instructions = si(Tail, []),
    Block = #x86_var_block{instructions=Instructions},
    #x86_var{body=[{start, Block}]}.

-spec si(cvar_tail(), [x86_var_instr()]) -> [x86_var_instr()].
si(#cvar_return{exp=CvarExp}, Instructions) ->
    Instructions ++
	si_assign(#reg{name=rax}, CvarExp) ++
	[#jmp{label=conclusion}];
si(#cvar_seq{stmt=#cvar_assign{var=Var, exp=CvarExp}, tail=Tail},
   Instructions) ->
    NewInstructions = 
	Instructions ++
	si_assign(#var_exp{name=Var}, CvarExp),
    si(Tail, NewInstructions).

-type assign_dest() :: #var_exp{} | #reg{}.
-spec si_assign(assign_dest(), cvar_exp()) -> [x86_var_instr()].
si_assign(Dest, #integer_const_exp{value=Int}) ->
    Mov = #x86_var_movq{dest=Dest, src=#imm{value=Int}},
    [Mov];
si_assign(Dest, Var=#var_exp{}) ->
    Mov = #x86_var_movq{dest=Dest, src=Var},
    [Mov];
si_assign(Dest, #read_exp{}) ->
    Call = #callq{label=read_int, value=0},
    Mov = #x86_var_movq{dest=Dest, src=#reg{name=rax}},
    [Call, Mov];
si_assign(Dest, #mon_negative_exp{body=MonAtom}) ->
    Arg = case MonAtom of
	      #var_exp{} -> MonAtom;
	      #integer_const_exp{value=Int} -> #imm{value=Int}
	  end,
    Neg = #x86_var_negq{dest=Arg},
    Mov = #x86_var_movq{dest=Dest, src=Arg},
    [Neg, Mov];
si_assign(Dest, #mon_plus_exp{left=Left, right=Right}) ->
    case Dest =:= Right of
	true ->
	    Add = #x86_var_addq{dest=Dest, src=build_arg(Left)},
	    [Add];
	false ->
	    Mov = #x86_var_movq{dest=Dest, src=build_arg(Left)},
	    Add = #x86_var_addq{dest=Dest, src=build_arg(Right)},
	    [Mov, Add]
    end;
si_assign(Dest, #mon_sub_exp{left=Left, right=Right}) ->
    case Dest =:= Left of
	true ->
	    Sub = #x86_var_subq{dest=Dest, src=build_arg(Right)},
	    [Sub];
	false ->
	    Mov = #x86_var_movq{dest=Dest, src=build_arg(Left)},
	    Sub = #x86_var_subq{dest=Dest, src=build_arg(Right)},
	    [Mov, Sub]
    end.

-spec build_arg(cvar_atom()) -> #imm{} | #var_exp{}.
build_arg(V=#var_exp{}) -> V;
build_arg(#integer_const_exp{value=Int}) -> #imm{value=Int}.
    
