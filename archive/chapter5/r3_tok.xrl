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

#t                      : {token, '#t'}.
#f                      : {token, '#f'}.
and                     : {token, 'and'}.
not                     : {token, 'not'}.
if                      : {token, 'if'}.
eq[?]                   : {token, 'eq?'}.
<                       : {token, '<'}.
>                       : {token, '>'}.
<=                      : {token, '<='}.
>=                      : {token, '>='}.

vector                  : {token, 'vector'}.
vector-ref              : {token, 'vector-ref'}.
vector-set!             : {token, 'vector-set'}.
void                    : {token, 'void'}.

let                     : {token, 'let'}.
[_a-zA-Z][_a-zA-Z0-9]*  : {token, {id, list_to_atom(TokenChars)}}.

\t                      : skip_token.
\n                      : skip_token.
\s                      : skip_token.


Erlang code.
