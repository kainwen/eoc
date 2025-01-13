-module(patch_instructions).

-export([patch_instructions/1]).

-include("ast.hrl").

-spec patch_instructions(x86_int()) -> x86_int().
patch_instructions(#x86_int{body=[{Label, Block}]}) ->
    NewBlock = pi(Block),
    #x86_int{body=[{Label, NewBlock}]}.

-spec pi(#x86_int_block{}) -> #x86_int_block{}.
pi(#x86_int_block{instructions=Instructions}) ->
    NewInstructions = lists:flatmap(fun pi_instr/1, Instructions),
    #x86_int_block{instructions=NewInstructions}.

-spec pi_instr(x86_int_instr()) -> [x86_int_instr()].
pi_instr(I=#x86_int_addq{dest=Dest, src=Src}) ->
    case {is_record(Dest, deref), is_record(Src, deref)} of
	{true, true} ->
	    Reg = #reg{name=rax},
	    Mov = #x86_int_movq{dest=Reg, src=Src},
	    Add = #x86_int_addq{dest=Dest, src=Reg},
	    [Mov, Add];
	_ -> [I]
    end;
pi_instr(I=#x86_int_subq{dest=Dest, src=Src}) ->
    case {is_record(Dest, deref), is_record(Src, deref)} of
	{true, true} ->
	    Reg = #reg{name=rax},
	    Mov = #x86_int_movq{dest=Reg, src=Src},
	    Sub = #x86_int_subq{dest=Dest, src=Reg},
	    [Mov, Sub];
	_ -> [I]
    end;
pi_instr(I=#x86_int_movq{dest=Dest, src=Src}) ->
    case {is_record(Dest, deref), is_record(Src, deref)} of
	{true, true} ->
	    Reg = #reg{name=rax},
	    Mov = #x86_int_movq{dest=Reg, src=Src},
	    Mov1 = #x86_int_movq{dest=Dest, src=Reg},
	    [Mov, Mov1];
	_ -> [I]
    end;
pi_instr(I) -> [I].

