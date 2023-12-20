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
