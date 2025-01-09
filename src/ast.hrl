-record(program, {body::exp()}).
-record(integer_const_exp, {value::integer()}).
-record(read_exp, {}).
-record(negative_exp, {body::exp()}).
-record(plus_exp, {left::exp(), right::exp()}).
-record(sub_exp, {left::exp(), right::exp()}).
-record(var_exp, {name::atom()}).
-type binding() :: {atom(), exp()}.
-record(let_exp, {bindings::[binding()], body::exp()}).

-type program() :: #program{}.
-type integer_const_exp() :: #integer_const_exp{}.
-type read_exp() :: #read_exp{}.
-type negative_exp() :: #negative_exp{}.
-type plus_exp() :: #plus_exp{}.
-type sub_exp() :: #sub_exp{}.
-type var_exp() :: #var_exp{}.
-type let_exp() :: #let_exp{}.
-type exp() :: integer_const_exp()
	     | read_exp()
	     | negative_exp()
	     | plus_exp()
	     | sub_exp()
	     | var_exp()
	     | let_exp().

-record(mon_negative_exp, {body::mon_atom_exp()}).
-record(mon_plus_exp, {left::mon_atom_exp(), right::mon_atom_exp()}).
-record(mon_sub_exp, {left::mon_atom_exp(), right::mon_atom_exp()}).
-record(mon_let_exp, {bindings::[mon_binding()], body::mon_exp()}).
-record(mon_program, {body::mon_exp()}).

-type mon_program() :: #mon_program{}.
-type mon_var_exp() :: var_exp().
-type mon_read_exp() :: read_exp().
-type mon_int_exp() :: integer_const_exp().
-type mon_negative_exp() :: #mon_negative_exp{}.
-type mon_plus_exp() :: #mon_plus_exp{}.
-type mon_sub_exp() :: #mon_sub_exp{}.
-type mon_let_exp() :: #mon_let_exp{}.
-type mon_binding() :: {atom(), mon_exp()}.

-type mon_atom_exp() :: mon_var_exp() | mon_int_exp().
-type mon_exp() :: mon_atom_exp() 
		 | mon_read_exp()
		 | mon_negative_exp()
		 | mon_plus_exp()
		 | mon_sub_exp()
		 | mon_let_exp().

-type cvar_label() :: atom().
-record(cvar_program, {body::[{cvar_label(), cvar_tail()}]}).
-type cvar_program() :: #cvar_program{}.
-record(cvar_assign, {var::atom(), exp::cvar_exp()}).
-record(cvar_return, {exp::cvar_exp()}).
-record(cvar_seq, {stmt::cvar_stmt(), tail::cvar_tail()}). 
-type cvar_atom() :: mon_int_exp() | mon_var_exp().
-type cvar_read_exp() :: mon_read_exp().
-type cvar_negative_exp() :: mon_negative_exp().
-type cvar_plus_exp() :: mon_plus_exp().
-type cvar_sub_exp() :: mon_sub_exp().
-type cvar_exp() :: cvar_atom()
		  | cvar_read_exp()
		  | cvar_negative_exp()
		  | cvar_plus_exp()
		  | cvar_sub_exp().
-type cvar_stmt() :: #cvar_assign{}.
-type cvar_tail() :: #cvar_return{} | #cvar_seq{}.

%% x86
-type regname() :: rsp | rbp | rax | rbx | rcx | rdx
		 | rsi | rdi | r8 | r9 | r10 | r11
		 | r12 | r13 | r14 | r15.
-record(imm, {value::integer()}).
-record(reg, {name::regname()}).
-record(deref, {reg::#reg{}, offset::integer()}).
-type x86_int_arg() :: #imm{} | #reg{} | #deref{}.
-type x86_var_arg() :: #imm{} | #reg{} | #deref{} | #var_exp{}.
-record(x86_var_addq, {dest::x86_var_arg(), src::x86_var_arg()}).
-record(x86_var_subq, {dest::x86_var_arg(), src::x86_var_arg()}).
-record(x86_var_negq, {dest::x86_var_arg()}).
-record(x86_var_movq, {dest::x86_var_arg(), src::x86_var_arg()}).
-record(x86_var_pushq, {arg::x86_var_arg()}).
-record(x86_var_popq, {arg::x86_var_arg()}).
-record(x86_int_addq, {dest::x86_int_arg(), src::x86_int_arg()}).
-record(x86_int_subq, {dest::x86_int_arg(), src::x86_int_arg()}).
-record(x86_int_negq, {dest::x86_int_arg()}).
-record(x86_int_movq, {dest::x86_int_arg(), src::x86_int_arg()}).
-record(x86_int_pushq, {arg::x86_int_arg()}).
-record(x86_int_popq, {arg::x86_int_arg()}).
-record(callq, {label::atom(), value::integer()}).
-record(retq, {}).
-record(jmp, {label::atom()}).
-type x86_var_instr() :: #x86_var_addq{} | #x86_var_subq{}
		       | #x86_var_negq{} | #x86_var_movq{}
		       | #x86_var_pushq{} | #x86_var_popq{}
		       | #callq{} | #retq{} | #jmp{}.
-type x86_int_instr() :: #x86_int_addq{} | #x86_int_subq{}
		       | #x86_int_negq{} | #x86_int_movq{}
		       | #x86_int_pushq{} | #x86_int_popq{}
		       | #callq{} | #retq{} | #jmp{}.
-record(x86_var_block, {instructions::[x86_var_instr()]}).
-record(x86_int_block, {instructions::[x86_int_instr()]}).
-record(x86_var, {body::[{atom(), #x86_var_block{}}]}).
-record(x86_int, {body::[{atom(), #x86_int_block{}}]}).
-type x86_var() :: #x86_var{}.
-type x86_int() :: #x86_int{}.
