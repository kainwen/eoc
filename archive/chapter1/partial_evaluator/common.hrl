-type exp() :: {int, integer()}
             | {read}
             | {minus, exp()}
             | {plus, exp(), exp()}.

-type program() :: {program, exp()}.

-type token() :: '(' | ')' | 'read' | '+' | '-' | {integer, integer()}.
