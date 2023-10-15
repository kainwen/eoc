%%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for R1 %%%%%%%%%%%%%%%%%%%%%%%%%
-type r1_program() :: {r1_program, r1_exp()}.

-type r1_exp() :: {int, integer()}
                | {read}
                | {minus, r1_exp()}
                | {plus, r1_exp(), r1_exp()}
                | {var, atom()}
                | {'let', [atom()], [r1_exp()], r1_exp()}.

-type r1_token() :: 'program' | '(' | ')' | 'read' | '+' | 'let'
                  | '-' | {integer, integer()} | {id, atom()}.

%%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for C0 %%%%%%%%%%%%%%%%%%%%%%%%%
-type c0_program() :: {c0_program, [c0_var()], [c0_statement()]}.

-type c0_statement() :: {assign, c0_var(), c0_exp()}
                      | {return, c0_arg()}.

-type c0_exp() :: c0_arg()
                | {read}
                | {minus, c0_arg()}
                | {plus, c0_arg(), c0_arg()}.

-type c0_arg() :: {int, integer()} | c0_var().

-type c0_var() :: atom().

%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for X86-star %%%%%%%%%%%%%%%%%%%%%%%%
-type x86_64_star_program() :: {x86_64_star_program, [x86_64_star_var()], [x86_64_star_inst()]}.

-type x86_64_star_label() :: atom().

-type x86_64_star_inst() :: {addq, x86_64_star_arg(), x86_64_star_arg()}
                          | {subq, x86_64_star_arg(), x86_64_star_arg()}
                          | {negq, x86_64_star_arg()}
                          | {movq, x86_64_star_arg(), x86_64_star_arg()}
                          | {callq, x86_64_star_label()}
                          | {pushq, x86_64_star_arg()}
                          | {popq, x86_64_star_arg()}
                          | {retq}.

-type x86_64_star_arg() :: {int, integer()}
                         | {register, reg()}
                         | x86_64_star_var().

-type x86_64_star_var() :: atom().

%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for X86-theta %%%%%%%%%%%%%%%%%%%%%%%%
-type x86_64_theta_program() :: {x86_64_theta_program,
                                 {[x86_64_theta_var()], [[x86_64_theta_var()]]},
                                 [x86_64_theta_inst()]}.

-type x86_64_theta_label() :: atom().

-type x86_64_theta_inst() :: {addq, x86_64_theta_arg(), x86_64_theta_arg()}
                           | {subq, x86_64_theta_arg(), x86_64_theta_arg()}
                           | {negq, x86_64_theta_arg()}
                           | {movq, x86_64_theta_arg(), x86_64_theta_arg()}
                           | {callq, x86_64_theta_label()}
                           | {pushq, x86_64_theta_arg()}
                           | {popq, x86_64_theta_arg()}
                           | {retq}.

-type x86_64_theta_arg() :: {int, integer()}
                          | {register, reg()}
                          | x86_64_theta_var().

-type x86_64_theta_var() :: atom().

%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for X86-gamma %%%%%%%%%%%%%%%%%%%%%%%%
-type x86_64_gamma_program() :: {x86_64_gamma_program,
                                 {[x86_64_gamma_var()],
                                  digraph:graph(),
                                  sets:set()},
                                 [x86_64_gamma_inst()]}.

-type x86_64_gamma_label() :: atom().

-type x86_64_gamma_inst() :: {addq, x86_64_gamma_arg(), x86_64_gamma_arg()}
                           | {subq, x86_64_gamma_arg(), x86_64_gamma_arg()}
                           | {negq, x86_64_gamma_arg()}
                           | {movq, x86_64_gamma_arg(), x86_64_gamma_arg()}
                           | {callq, x86_64_gamma_label()}
                           | {pushq, x86_64_gamma_arg()}
                           | {popq, x86_64_gamma_arg()}
                           | {retq}.

-type x86_64_gamma_arg() :: {int, integer()}
                          | {register, reg()}
                          | x86_64_gamma_var().

-type x86_64_gamma_var() :: atom().

%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for X86-mu %%%%%%%%%%%%%%%%%%%%%%%%
-type x86_64_mu_program() :: {x86_64_mu_program,
                              [{x86_64_mu_var(), x86_64_mu_reg_result()}],
                              [x86_64_mu_inst()]}.

-type x86_64_mu_label() :: atom().

-type x86_64_mu_inst() :: {addq, x86_64_mu_arg(), x86_64_mu_arg()}
                        | {subq, x86_64_mu_arg(), x86_64_mu_arg()}
                        | {negq, x86_64_mu_arg()}
                        | {movq, x86_64_mu_arg(), x86_64_mu_arg()}
                        | {callq, x86_64_mu_label()}
                        | {pushq, x86_64_mu_arg()}
                        | {popq, x86_64_mu_arg()}
                        | {retq}.

-type x86_64_mu_arg() :: {int, integer()}
                       | {register, reg()}
                       | x86_64_mu_var().

-type x86_64_mu_var() :: atom().

-type x86_64_mu_reg_result() :: {register, reg()} | integer().

%%%%%%%%%%%%%%%%%%%%%%%%% Page 18 X86 subset %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%reg ::= rsp | rbp | rax | rbx | rcx | rdx | rsi
%%      | rdi | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
%%arg ::= $int | %reg | int(%reg)
%%instr ::= addq arg, arg | subq arg, arg | negq arg | movq arg, arg
%%        | callq label | pushq arg | popq arg | retq
%%prog ::= .globl main
%%         main: instr +
%%%%%%%%%%%%% The Abstract Syntax Tree of language above%%%%%%%%%%%%%%%%%%%%%%
-type x86_64_program() :: {program, integer(), [instruction()]}.

-type x86_64_label() :: atom().

-type instruction() :: {addq, x86_64_arg(), x86_64_arg()}
                     | {subq, x86_64_arg(), x86_64_arg()}
                     | {negq, x86_64_arg()}
                     | {movq, x86_64_arg(), x86_64_arg()}
                     | {callq, x86_64_label()}
                     | {pushq, x86_64_arg()}
                     | {popq, x86_64_arg()}
                     | {retq}.

-type x86_64_arg() :: {int, integer()}
                    | {register, reg()}
                    | {deref, integer(), reg()}.

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
             | r15.% callee saved
