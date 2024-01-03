-type binding() :: {atom(), exp()}.

-record(integer_const_exp, {value::integer()}).
-record(read_exp, {}).
-record(negative_exp, {body::exp()}).
-record(plus_exp, {left::exp(), right::exp()}).
-record(sub_exp, {left::exp(), right::exp()}).
-record(var_exp, {name::atom()}).
-record(let_exp, {bindings::[binding()], body::exp()}).

-type exp() :: #integer_const_exp{}
	     | #read_exp{}
	     | #negative_exp{}
	     | #plus_exp{}
	     | #sub_exp{}
	     | #var_exp{}
	     | #let_exp{}.

-record(program, {body::exp()}).
-type program() :: #program{}.

-record(int_value, {value::integer()}).
-type value() :: #int_value{}.

-type binop() :: '+' | '-'.

-type env() :: [{atom(), value()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type regname() :: rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi
		 | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15.

-record(imm, {value::integer()}).
-record(reg, {regname::regname()}).
-record(deref, {base::#reg{}, offset::integer()}).

-type arg() :: #imm{} | #reg{} | #deref{}.

-type label() :: {label, atom()}.

-record(addq, {src::arg(), dest::arg()}).
-record(subq, {src::arg(), dest::arg()}).
-record(negq, {dest::arg()}).
-record(movq, {src::arg(), dest::arg()}).
-record(pushq, {src::arg()}).
-record(popq, {dest::arg()}).
-record(callq, {label::label(), nargs::integer()}).
-record(retq, {}).
-record(jmp, {label::label()}).

-type instruction() :: #addq{} | #subq{} | #negq{} | #movq{} | #pushq{}
		     | #popq{} | #callq{} | #retq{} | #jmp{}.

-record(block, {body::[instruction()]}).
-record(x86_prog, {body::[{label(), #block{}}]}).

-type x86_prog() :: #x86_prog{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Type Specification for CVar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(cvar_int, {value::integer()}).
-record(cvar_var, {var::atom()}).
-type cvar_atm() :: #cvar_int{} | #cvar_var{}.
-record(cvar_read, {}).
-record(cvar_neg, {arg::cvar_atm()}).
-record(cvar_plus, {left::cvar_atm(), right::cvar_atm()}).
-record(cvar_sub, {left::cvar_atm(), right::cvar_atm()}).
-type cvar_exp() :: #cvar_read{} | #cvar_neg{} |
		    #cvar_plus{} | #cvar_sub{} | cvar_atm().
-record(cvar_assign, {var::atom(), exp::cvar_exp()}).
-type cvar_stmt() :: #cvar_assign{}.
-record(cvar_return, {exp::cvar_exp()}).
-record(cvar_seq, {stmt::cvar_stmt(), tail::cvar_tail()}).
-type cvar_tail() :: #cvar_return{} | #cvar_seq{}.
-record(cvar_prog, {body::[{label(), cvar_tail()}]}).
-type cvar_prog() :: #cvar_prog{}.

%% Types for counter
-type counter() :: pid().
