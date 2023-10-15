Definitions.


Rules.

program                 : {token, 'program'}.

[()]                    : {token, list_to_atom(TokenChars)}.

read                    : {token, 'read'}.

[+]                     : {token, '+'}.
-                       : {token, '-'}.
[[]                     : {token, '('}.
[]]                     : {token, ')'}.

-?[0-9]+                : {token, {integer, list_to_integer(TokenChars)}}.

let                     : {token, 'let'}.
[_a-zA-Z][_a-zA-Z0-9]*  : {token, {id, list_to_atom(TokenChars)}}.

\t                      : skip_token.
\n                      : skip_token.
\s                      : skip_token.


Erlang code.
