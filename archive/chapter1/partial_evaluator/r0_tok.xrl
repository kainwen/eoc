Definitions.


Rules.

[()]                    : {token, list_to_atom(TokenChars)}.

-?[0-9]+                : {token, {integer, list_to_integer(TokenChars)}}.
read                    : {token, 'read'}.

[+]                     : {token, '+'}.
-                       : {token, '-'}.

\t                      : skip_token.
\n                      : skip_token.
\s                      : skip_token.


Erlang code.
