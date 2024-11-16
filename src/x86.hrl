-record(label, {name::atom()}).

-record(register, {name::atom()}).
-record(imm, {value::integer()}).
-record(deref, {reg::#register{}, offset::integer()}).

-type instr_arg() :: #register{} | #imm{} | #deref{}.

-record(instr_add, {arg1::instr_arg(), arg2::instr_arg()}).
-record(instr_sub, {arg1::instr_arg(), arg2::instr_arg()}).
-record(instr_neg, {arg::instr_arg()}).
-record(instr_mov, {arg1::instr_arg(), arg2::instr_arg()}).
-record(instr_push, {arg::instr_arg()}).
-record(instr_pop, {arg::instr_arg()}).
-record(instr_ret, {}).
-record(instr_jmp, {label::#label{}}).
-record(instr_call, {func::#label{}, nargs::integer()}).

-type instr() :: #instr_add{} | #instr_sub{} | #instr_neg{} | #instr_mov{}
	       | #instr_push{} | #instr_pop{} | #instr_ret{} | #instr_jmp{}
	       | #instr_call{}.

-record(block, {info::info(), instructions::[instr()]}).
-record(x86prog, {info::info(), body::[{#label{}, #block{}}].

-type block() :: #block{}.
-type x86prog() :: #x86prog{}.
