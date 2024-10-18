-module(pe_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

pe_test() ->
    Code = "(program ()
             (let
               ([a 1]
                [b (let ([x 1]) (+ x x))])
               (- (read) (- (+ a b)))))",
    Prog = parse:scan_and_parse(Code),
    P = pe:partial_eval(Prog),
    ?assert(P =:= {program,{sub_exp,{read_exp},{integer_const_exp,-3}}}),
    ok.

-endif.
