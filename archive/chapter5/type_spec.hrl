%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree of R3 %%%%%%%%%%%%%%%%%%%%%%
-type r3_program() :: {r3_program, r3_exp()}.

-type r3_cmp() :: 'eq?' | '<=' | '>=' | '<' | '>'.

-type r3_exp() :: {int, integer()}
                | {read}
                | {minus, r3_exp()}
                | {plus, r3_exp(), r3_exp()}
                | {var, atom()}
                | {'let', [atom()], [r3_exp()], r3_exp()}
                | {true_exp}
                | {false_exp}
                | {and_exp, r3_exp(), r3_exp()}
                | {not_exp, r3_exp()}
                | {{cmp, r3_cmp()}, r3_exp(), r3_exp()}
                | {if_exp, r3_exp(), r3_exp(), r3_exp()}
                | {void}
                | {tuple_exp, [r3_exp()]}
                | {vector_set_exp, r3_exp(), integer(), r3_exp()}
                | {vector_ref_exp, r3_exp(), integer()}.

-type r3_token() :: 'program' | '(' | ')' | 'read' | '[' | ']'
                  | '+' | {integer, integer()} | {id, atom()}
                  | '#t' | '#f' | 'and' | 'not' | 'if' | 'eq?'
                  | '<' | '>' | '<=' | '>=' | 'let' | 'void'
                  | 'vector' | 'vector-ref' | 'vector-set!'.

%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree of R3_ext %%%%%%%%%%%%%%%%%%%%%%
-type r3_ext_program() :: {r3_ext_program, r3_ext_exp()}.

-type r3_ext_cmp() :: 'eq?' | '<=' | '>=' | '<' | '>'.

-type r3_ext_exp() :: {int, integer()}
                    | {read}
                    | {minus, r3_ext_exp()}
                    | {plus, r3_ext_exp(), r3_ext_exp()}
                    | {var, atom()}
                    | {'let', [atom()], [r3_ext_exp()], r3_ext_exp()}
                    | {true_exp}
                    | {false_exp}
                    | {and_exp, r3_ext_exp(), r3_ext_exp()}
                    | {not_exp, r3_ext_exp()}
                    | {{cmp, r3_ext_cmp()}, r3_ext_exp(), r3_ext_exp()}
                    | {if_exp, r3_ext_exp(), r3_ext_exp(), r3_ext_exp()}
                    | {void}
                    | {vector_set_exp, r3_ext_exp(), integer(), r3_ext_exp()}
                    | {vector_ref_exp, r3_ext_exp(), integer()}
                    | {collect, integer()}
                    | {allocate, integer(), place_holder}
                    | {global_value, atom()}.

%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree of R3_tp %%%%%%%%%%%%%%%%%%%%%%
-type r3_tp_program() :: {r3_tp_program, {type, r3_tp_type()}, r3_tp_exp()}.

-type r3_tp_cmp() :: 'eq?' | '<=' | '>=' | '<' | '>'.

-type r3_tp_type() :: int | bool | {tuple, [r3_tp_type()]} | void.

-type r3_tp_exp() :: {has_type, {int, integer()}, int}
                   | {has_type, {read}, int}
                   | {has_type, {minus, r3_tp_exp()}, int}
                   | {has_type, {plus, r3_tp_exp(), r3_tp_exp()}, int}
                   | {has_type, {var, atom()}, r3_tp_type()}
                   | {has_type,
                      {'let',
                       [atom()],
                       [r3_tp_exp()],
                       r3_tp_exp()},
                      r3_tp_type()}
                   | {has_type, {true_exp}, bool}
                   | {has_type, {false_exp}, bool}
                   | {has_type, {and_exp, r3_tp_exp(), r3_tp_exp()}, bool}
                   | {has_type, {not_exp, r3_tp_exp()}, bool}
                   | {has_type,
                      {{cmp, r3_tp_cmp()},
                       r3_tp_exp(),
                       r3_tp_exp()},
                      bool}
                   | {has_type,
                      {if_exp,
                       r3_tp_exp(),
                       r3_tp_exp(),
                       r3_tp_exp()},
                      r3_tp_type()}
                   | {has_type, {void}, void}
                   | {has_type,
                      {vector_set_exp,
                       r3_tp_exp(),
                       integer(),
                       r3_tp_exp()},
                      r3_tp_type()}
                   | {has_type,
                      {vector_ref_exp,
                       r3_tp_exp(),
                       integer()},
                      r3_tp_type()}
                   | {has_type,
                      {collect, integer()},
                      void}
                   | {has_type,
                      {allocate, integer(), r3_tp_type()},
                      r3_tp_type()}
                   | {has_type,
                      {global_value, atom()},
                      r3_tp_type()}.

%%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for C2 %%%%%%%%%%%%%%%%%%%%%%%%%
-type c2_program() :: {c2_program,
                       [{c2_var(), c2_type()}],
                       {type, c2_type()},
                       [c2_statement()]}.

-type c2_var() :: atom().

-type c2_type() :: int | bool | void | {tuple, [c2_type()]}.

-type c2_exp() :: c2_arg() | {read} | {minus, c2_arg()}
                | {plus, c2_arg(), c2_arg()} | {not_op, c2_arg()}
                | {{cmp, c2_cmp()}, c2_arg(), c2_arg()}
                | {allocate, integer(), c2_type()}
                | {vector_ref, c2_arg(), integer()}
                | {vector_set, c2_arg(), integer(), c2_arg()}
                | {global_value, atom()}
                | {void}.

-type c2_statement() :: {assign, c2_var(), c2_exp()}
                      | {return, c2_arg()}
                      | {c2_if,
                         {{cmp, c2_cmp()}, c2_arg(), c2_arg()},
                         [c2_statement()],
                         [c2_statement()]}
                      | {collect, integer()}.

-type c2_arg() :: {int, integer()} | c2_var()
                | {bool, c2_true} | {bool, c2_false} | {void}.

-type c2_cmp() :: 'eq?' | '<' | '>' | '=<' | '>='.

%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for x86_star %%%%%%%%%%%%%%%%%%%%%%%
-type x86_star_program() :: {x86_star_program,
                             Var_type_list :: [{atom(), x86_star_type()}],
                             Program_return_type :: {type,x86_star_type()},
                             Insts :: [{x86_star_instruction()}]}.

-type x86_star_type() :: int | bool | void | {tuple, [x86_star_type()]}.

-type x86_star_instruction() :: {addq, x86_star_arg(), x86_star_arg()}
                              | {subq, x86_star_arg(), x86_star_arg()}
                              | {negq, x86_star_arg()}
                              | {movq, x86_star_arg(), x86_star_arg()}
                              | {callq, atom()}
                              | {pushq, x86_star_arg()}
                              | {popq, x86_star_arg()}
                              | {retq}
                              | {xorq, x86_star_arg(), x86_star_arg()}
                              | {cmpq, x86_star_arg(), x86_star_arg()}
                              | {set, cc(), x86_star_arg()}
                              | {movzbq, x86_star_arg(), x86_star_arg()}
                              | {jmp, atom()}
                              | {j, cc(), atom()}
                              | {label, atom()}
                              | {c2_if,
                                 {{cmp, c2_cmp()}, x86_star_arg(), x86_star_arg()},
                                 [x86_star_instruction()],
                                 [x86_star_instruction()]}.

-type x86_star_arg() :: atom()
                      | {int, integer()}
                      | {register, reg()}
                      | {deref, {register, reg()}, integer()}
                      | {byte_reg, {register, reg()}} | {global_value, atom()}.

-type cc() :: 'e' | 'l' | 'le' | 'g' | 'ge'.

%%%%%%%%%%%%%%%%%%%%%%% Abstract Syntax Tree for x86_star %%%%%%%%%%%%%%%%%%%%%%%
-type x86_gamma_program() :: {x86_gamma_program,
                              {frame_size, integer()},
                              {root_size, integer()},
                              [x86_gamma_instruction()]}.

-type x86_gamma_type() :: int | bool | void | {tuple, [x86_gamma_type()]}.

-type x86_gamma_instruction() :: {addq, x86_gamma_arg(), x86_gamma_arg()}
                               | {subq, x86_gamma_arg(), x86_gamma_arg()}
                               | {negq, x86_gamma_arg()}
                               | {movq, x86_gamma_arg(), x86_gamma_arg()}
                               | {callq, atom()}
                               | {pushq, x86_gamma_arg()}
                               | {popq, x86_gamma_arg()}
                               | {retq}
                               | {xorq, x86_gamma_arg(), x86_gamma_arg()}
                               | {cmpq, x86_gamma_arg(), x86_gamma_arg()}
                               | {set, cc(), x86_gamma_arg()}
                               | {movzbq, x86_gamma_arg(), x86_gamma_arg()}
                               | {jmp, atom()}
                               | {j, cc(), atom()}
                               | {label, atom()}.

-type x86_gamma_arg() :: {int, integer()}
                       | {register, reg()}
                       | {deref, {register, reg()}, integer()}
                       | {byte_reg, {register, reg()}} | {global_value, atom()}.

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
