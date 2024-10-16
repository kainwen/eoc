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
