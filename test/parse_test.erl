-module(parse_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% Insert tests here.
parse_test() ->
    Code = "(program ()
             (let
               ([a 1]
                [b (let ([x 1]) (+ x x))])
               (- (read) (- (+ a b)))))",
    Prog = parse:scan_and_parse(Code),
    ?assert(Prog =:=
		{program,{let_exp,[{a,{integer_const_exp,1}},
				   {b,{let_exp,[{x,{integer_const_exp,1}}],
				       {plus_exp,{var_exp,x},{var_exp,x}}}}],
			  {sub_exp,{read_exp},
			   {negative_exp,{plus_exp,{var_exp,a},{var_exp,b}}}}}}),
    ok.

-endif.
