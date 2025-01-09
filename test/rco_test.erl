-module(rco_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

rco_test() ->
    Code = "(program ()
              (let ([x (+ 42 (- 10))])
                 (+ x 10)))",
    Prog = parse:scan_and_parse(Code),
    MonProg = rco:remove_complex_operands(Prog),
    ?assert(MonProg =:= {mon_program,
			 {mon_let_exp,
			  [{x,{mon_let_exp,
			       [{'tmp.1',{mon_negative_exp,
					  {integer_const_exp,10}}}],
			       {mon_plus_exp,
				{integer_const_exp,42},
				{var_exp,'tmp.1'}}}}],
			  {mon_plus_exp,
			   {var_exp,x},
			   {integer_const_exp,10}}}}),
    Code1 = "(program()
               (let ([a 42])
                  (let ([b a])
                    b)))",
    Prog1 = parse:scan_and_parse(Code1),
    MonProg1 = rco:remove_complex_operands(Prog1),
    ?assert(MonProg1 =:= {mon_program,
			  {mon_let_exp,
			   [{a,{integer_const_exp,42}}],
			   {mon_let_exp,
			    [{b,{var_exp,a}}],
			    {var_exp,b}}}}),
    ok.

-endif.
