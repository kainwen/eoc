-module(r0_test).

-include_lib("eunit/include/eunit.hrl").

r0_test() ->
    Test_files = [{"code/code-0", "code/code-input-0"},
                  {"code/code-1", "code/code-input-1"},
                  {"code/code-2", "code/code-input-2"}],
    [test(Code_file, Input_file) || {Code_file, Input_file} <- Test_files].

test(Code_file, Input_file) ->
    Ast0 = r0_parse:parse_file(Code_file),
    Ast1 = r0_pe:pe(Ast0),
    Ast2 = r0_pe_advanced:pe_advanced(Ast1),
    {ok, IO1} = file:open(Input_file, [read]),
    {ok, IO2} = file:open(Input_file, [read]),
    {ok, IO3} = file:open(Input_file, [read]),
    R1 = r0:eval(Ast0, IO1),
    R2 = r0:eval(Ast1, IO2),
    R3 = r0:eval(Ast2, IO3),
    ?assert(R1 =:= R2),
    ?assert(R1 =:= R3),
    file:close(IO1),
    file:close(IO2),
    file:close(IO3).
