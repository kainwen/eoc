Definitions.


Rules.

program                 : {token, {'program', TokenLine}}.

[()]                    : {token, {list_to_atom(TokenChars), TokenLine}}.

read                    : {token, {'read', TokenLine}}.

[+]                     : {token, {'+', TokenLine}}.
-                       : {token, {'-', TokenLine}}.
[[]                     : {token, {'(', TokenLine}}.
[]]                     : {token, {')', TokenLine}}.

-?[0-9]+                : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

let                     : {token, {'let', TokenLine}}.
[_a-zA-Z][._a-zA-Z0-9]* : {token, {id, TokenLine, list_to_atom(TokenChars)}}.

\t                      : skip_token.
\n                      : skip_token.
\s                      : skip_token.


Erlang code.
