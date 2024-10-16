Nonterminals prog
	     exp
             integer_const_exp read_exp negative_exp
             plus_exp sub_exp var_exp let_exp binding bindings.

Terminals program
          id
	  let
	  read
	  '(' ')'
	  '+' '-'
	  integer.

Rootsymbol prog.

prog ->
    '(' program '(' ')' exp ')':
	#program{body='$5'}.

exp ->
    integer_const_exp :
	'$1'.

exp ->
    read_exp :
	'$1'.

exp ->
    negative_exp :
	'$1'.

exp ->
    plus_exp :
	'$1'.

exp ->
    sub_exp :
	'$1'.

exp ->
    var_exp :
	'$1'.

exp ->
    let_exp :
	'$1'.

integer_const_exp ->
    integer :
	#integer_const_exp{value=element(3, '$1')}.

read_exp ->
    '(' read ')' :
	#read_exp{}.

negative_exp ->
    '(' '-' exp ')' :
	#negative_exp{body='$3'}.

plus_exp ->
    '(' '+' exp exp ')' :
	#plus_exp{left='$3', right='$4'}.

sub_exp ->
    '(' '-' exp exp ')' :
	#sub_exp{left='$3', right='$4'}.

var_exp ->
    id :
	#var_exp{name=element(3, '$1')}.

binding ->
    '(' id exp ')' :
	{element(3, '$2'), '$3'}.

bindings ->
    '$empty' :
	[].

bindings ->
    binding bindings :
	['$1'|'$2'].

let_exp ->
    '(' 'let' '(' bindings ')' exp ')' :
	ok = check_bindings('$4'),
	#let_exp{bindings='$4', body='$6'}.

Erlang code.    
-include("ast.hrl").

check_bindings(Bindings) -> 
    N1 = length(Bindings),    
    N2 = sets:size(sets:from_list([Id || {Id, _} <- Bindings])),
    case N1 =:= N2 of
	true ->
	    ok;
	false ->
	    erlang:error({"Duplicated Var names", Bindings})
    end.
