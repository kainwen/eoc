Chapter 4
=========

## File description

* `common.hrl`: type specs
* `r2_tok.erl` and `r2_parse.erl`: the compiler front-end
* `r2.erl`: the interpreter for R2 langauge
* `r2_type_checker.erl` and `senv.erl`: type checker for R2, static analyze a R2 program's type
* `uniquify.erl` and `name_server.erl`: the uniquify pass
* `flatten.erl` and `var_server.erl`: the flatten pass
* `select_instructions.erl`: the select_instructions pass
* `register_allocation.erl` : the register allocation pass, contains
  + `uncover_liveness` for live analyze
  + `build_intereference` and `allocate_registers` for graph_coloring
  + `assign homes` for replace vars with regsiters
* `lower_condition.erl` and `gensym.erl`: compile if to instructions
* `patch_instructions.erl`: patch instructions pass
* `print_x86.erl`: from x86_1 AST to string
* `compiler.erl`: compile from lisp to X86
