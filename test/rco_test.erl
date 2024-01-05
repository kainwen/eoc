-ifdef(TEST).
-module(rco_test).

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    Func = fun (CodeDir, ExpectedDir, CodeFn) ->
		   CodeAbsFn = filename:join(CodeDir, CodeFn),
		   AnsAbsFn = filename:join(ExpectedDir,
					    string:join([CodeFn, "rco"], ".")),
		   {ok, [Ans]} = file:consult(AnsAbsFn),
		   P = parse:scan_and_parse_file(CodeAbsFn),
		   ?assert(rco:remove_complex_operands(P) =:= Ans),
		   ok		   
	   end,
    ok = test_utils:test_framework(Func).

-endif.
