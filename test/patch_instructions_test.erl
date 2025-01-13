-module(patch_instructions_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

patch_instructions_test() ->
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
    XI = assign_homes:assign_homes(X),    
    XII = patch_instructions:patch_instructions(XI),
    ?assert(XII =:= {x86_int,
		     [{start,
		       {x86_int_block,
			[{x86_int_movq,{deref,{reg,rbp}, -8}, {imm,4}},
			 {x86_int_movq,{reg,rax},{deref,{reg,rbp},-8}},
			 {x86_int_movq,{deref,{reg,rbp},-16},{reg,rax}},
			 {x86_int_addq,{deref,{reg,rbp},-16},{imm,1}},
			 {x86_int_movq,{reg,rax},{deref,{reg,rbp},-16}},
			 {x86_int_movq,{deref,{reg,rbp},-24},{reg,rax}},
			 {x86_int_addq,{deref,{reg,rbp},-24},{imm,2}},
			 {callq,read_int,0},
			 {x86_int_movq,{deref,{reg,rbp},-32},{reg,rax}},
			 {x86_int_negq,{deref,{reg,rbp},-32}},
			 {x86_int_movq,{reg,rax},{deref,{reg,rbp},-32}},
			 {x86_int_movq,{deref,{reg,rbp},-40},{reg,rax}},
			 {x86_int_movq,{reg,rax},{deref,{reg,rbp},-24}},
			 {x86_int_addq,{reg,rax},{deref,{reg,rbp},-40}},
			 {jmp,conclusion}]}}]}),
    ok.

-endif.
