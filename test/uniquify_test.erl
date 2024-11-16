-module(uniquify_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

uniquify_test() ->
    Code1 = "(program () (let ([x 32]) (+ (let ([x 10]) x) x)))", 
    ?assert(uniquify:uniquify(parse:scan_and_parse(Code1)) =:=
		{program,{let_exp,[{'x.1',{integer_const_exp,32}}],
			  {plus_exp,{let_exp,[{'x.2',{integer_const_exp,10}}],
                                     {var_exp,'x.2'}},
			   {var_exp,'x.1'}}}}),

    Code2 = "(program () (let ([x (let ([x 4]) (+ x 1))]) (+ x 2)))",
    ?assert(uniquify:uniquify(parse:scan_and_parse(Code2)) =:=
		{program,{let_exp,[{'x.2',{let_exp,[{'x.1',{integer_const_exp,4}}],
					   {plus_exp,{var_exp,'x.1'},{integer_const_exp,1}}}}],
			  {plus_exp,{var_exp,'x.2'},{integer_const_exp,2}}}}),
    ok.

-endif.
