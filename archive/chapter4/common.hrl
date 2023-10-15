%%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for R1 %%%%%%%%%%%%%%%%%%%%%%%%%
-type r2_program() :: {r2_program, {type, r2_type()}, r2_exp()}.

-type r2_cmp() :: 'eq?' | '<=' | '>=' | '<' | '>'.

-type r2_exp() :: {int, integer()}
                | {read}
                | {minus, r2_exp()}
                | {plus, r2_exp(), r2_exp()}
                | {var, atom()}
                | {'let', [atom()], [r2_exp()], r2_exp()}
                | {true_exp}
                | {false_exp}
                | {and_exp, r2_exp(), r2_exp()}
                | {not_exp, r2_exp()}
                | {{cmp, r2_cmp()}, r2_exp(), r2_exp()}
                | {if_exp, r2_exp(), r2_exp(), r2_exp()}.

-type r2_token() :: 'program' | '(' | ')' | 'read' | '[' | ']'
                  | '+' | {integer, integer()} | {id, atom()}
                  | '#t' | '#f' | 'and' | 'not' | 'if' | 'eq?'
                  | '<' | '>' | '<=' | '>=' | 'let'.

-type r2_type() :: int | bool.

%%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for C1 %%%%%%%%%%%%%%%%%%%%%%%%%
-type c1_program() :: {c1_program,
                       {[c1_var()], {type, c1_type()}},
                       [c1_statement()]}.

-type c1_var() :: atom().

-type c1_type() :: int | bool.

-type c1_exp() :: c1_arg() | {read} | {minus, c1_arg()}
                | {plus, c1_arg(), c1_arg()} | {not_op, c1_arg()}
                | {{cmp, c1_cmp()}, c1_arg(), c1_arg()}.

-type c1_statement() :: {assign, c1_var(), c1_exp()}
                      | {return, c1_arg()}
                      | {c1_if,
                         {{cmp, c1_cmp()}, c1_arg(), c1_arg()},
                         [c1_statement()],
                         [c1_statement()]}.

-type c1_arg() :: {int, integer()} | c1_var() | {bool, c1_true} | {bool, c1_false}.

-type c1_cmp() :: 'eq?' | '<' | '>' | '=<' | '>='.

%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for x86_star %%%%%%%%%%%%%%%%%%%%%%%
-type x86_star_program() :: {x86_star_program,
                             [x86_star_info()],
                             {type, x86_star_type()},
                             [x86_star_instruction()]}.

-type x86_star_info() :: x86_star_var() | {frame_size, integer()}.

-type x86_star_var() :: atom().

-type x86_star_type() :: int | bool.

-type x86_star_instruction() :: {addq, x86_star_arg(), x86_star_arg()}
                              | {subq, x86_star_arg(), x86_star_arg()}
                              | {negq, x86_star_arg()}
                              | {movq, x86_star_arg(), x86_star_arg()}
                              | {callq, x86_star_label()}
                              | {pushq, x86_star_arg()}
                              | {popq, x86_star_arg()}
                              | {retq}
                              | {xorq, x86_star_arg(), x86_star_arg()}
                              | {cmpq, x86_star_arg(), x86_star_arg()}
                              | {set, x86_star_cond_code(), x86_star_arg()}
                              | {movzbq, x86_star_arg(), x86_star_arg()}
                              | {jmp, x86_star_label()}
                              | {jmp_if, x86_star_cond_code(), x86_star_label()}
                              | {label, x86_star_label()}
                              | tmp_if_statement()
                              | tmp_if_with_live_vars().

-type tmp_if_with_live_vars() ::{c1_if,
                                  {{cmp, c1_cmp()},
                                   x86_star_arg(),
                                   x86_star_arg()},
                                 {[x86_star_instruction()], [x86_star_var()]},
                                 {[x86_star_instruction()], [x86_star_var()]}}.

-type tmp_if_statement() :: {c1_if,
                             {{cmp, c1_cmp()}, x86_star_arg(), x86_star_arg()},
                             [x86_star_instruction()],
                             [x86_star_instruction()]}.

-type x86_star_cond_code() :: e | l | le | g | ge.

-type x86_star_label() :: atom().

-type x86_star_arg() :: {int, integer()}
                      | x86_star_var()
                      | {register, reg()}
                      | {byte_reg, {register, reg()}}.

-type register_allocate_result() :: {register, reg()} | integer().

%%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for x86_1 %%%%%%%%%%%%%%%%%%%%%%%%%
-type x86_1_program() :: {x86_1_program,
                          x86_1_info(),
                          {type, x86_1_type()},
                          [x86_1_instruction()]}.

-type x86_1_instruction() :: {addq, x86_1_arg(), x86_1_arg()}
                           | {subq, x86_1_arg(), x86_1_arg()}
                           | {negq, x86_1_arg()}
                           | {movq, x86_1_arg(), x86_1_arg()}
                           | {callq, x86_1_label()}
                           | {pushq, x86_1_arg()}
                           | {popq, x86_1_arg()}
                           | {retq}
                           | {xorq, x86_1_arg(), x86_1_arg()}
                           | {cmpq, x86_1_arg(), x86_1_arg()}
                           | {set, x86_1_cond_code(), x86_1_arg()}
                           | {movzbq, x86_1_arg(), x86_1_arg()}
                           | {jmp, x86_1_label()}
                           | {jmp_if, x86_1_cond_code(), x86_1_label()}
                           | {label, x86_1_label()}.

-type x86_1_cond_code() :: e | l | le | g | ge.

-type x86_1_info() :: any().

-type x86_1_label() :: atom().

-type x86_1_arg() :: {int, {integer()}}
                   | {register, reg()}
                   | {deref, {register, reg()}, integer()}
                   | {byte_reg, {register, reg()}}.

-type x86_1_type() :: int | bool.

%% comment each reg by usage convention on P180 CSAPP 3rd.
-type reg() :: rsp % stack pointer
             | rbp % callee saved
             | rax % return value
             | rbx % callee saved
             | rcx % the 4th x86_64_argument
             | rdx % the 3rd x86_64_argument
             | rsi % the 2nd x86_64_argument
             | rdi % the 1st x86_64_argument
             | r8  % the 5th x86_64_argument
             | r9  % the 6th x86_64_argument
             | r10 % caller saved
             | r11 % caller saved
             | r12 % callee saved
             | r13 % callee saved
             | r14 % callee saved
             | r15 % callee saved
             | al. % byte register
