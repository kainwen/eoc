-module(explicate_control_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

explicate_control_test() ->
    Code = "(program ()
             (let ([y (let ([a 20])
               (let ([b 10])
                 (+ a b)))])
             y))",
    P = parse:scan_and_parse(Code),
    Mp = rco:remove_complex_operands(P),
    Cp = explicate_control:explicate_control(Mp),
    ?assert(Cp =:= {cvar_program,[{start,
				   {cvar_seq,
				    {cvar_assign,a,
				     {integer_const_exp,20}},
				    {cvar_seq,
				     {cvar_assign,b,
				      {integer_const_exp,10}},
				     {cvar_seq,
				      {cvar_assign,y,
				       {mon_plus_exp,{var_exp,a},{var_exp,b}}},
				      {cvar_return,{var_exp,y}}}}}}]}),
    ok.

-endif.
