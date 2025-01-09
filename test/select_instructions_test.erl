-module(select_instructions_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

select_instructions_test() ->
    Code = "(program
               ()
               (let
                 ([x (let ([x 4])
                        (+ x 1))])
                 (+ (+ x 2)
                    (- (read)))))",
    P = parse:scan_and_parse(Code),
    Up = uniquify:uniquify(P),    
    Mp = rco:remove_complex_operands(Up),
    Cp = explicate_control:explicate_control(Mp),
    X = select_instructions:select_instructions(Cp),
    ?assert(X =:=
		{x86_var,[{start,{x86_var_block,
				  [{x86_var_movq,{var_exp,'x.1'},
				    {imm,4}},
				   {x86_var_movq,{var_exp,'x.2'},{var_exp,'x.1'}},
				   {x86_var_addq,{var_exp,'x.2'},{imm,1}},
				   {x86_var_movq,{var_exp,'tmp.2'},{var_exp,'x.2'}},
				   {x86_var_addq,{var_exp,'tmp.2'},{imm,2}},
				   {callq,read_int,0},
				   {x86_var_movq,{var_exp,'tmp.1'},{reg,rax}},
				   {x86_var_negq,{var_exp,'tmp.1'}},
				   {x86_var_movq,{var_exp,'tmp.3'},{var_exp,'tmp.1'}},
				   {x86_var_movq,{reg,rax},{var_exp,'tmp.2'}},
				   {x86_var_addq,{reg,rax},{var_exp,'tmp.3'}},
				   {jmp,conclusion}]}}]}),
    ok.

-endif.

