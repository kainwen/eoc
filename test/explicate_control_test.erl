-ifdef(TEST).
-module(explicate_control_test).

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    Func = fun (CodeDir, ExpectedDir, CodeFn) ->
		   CodeAbsFn = filename:join(CodeDir, CodeFn),
		   AnsAbsFn = filename:join(ExpectedDir,
					    string:join([CodeFn, "ec"], ".")),
		   {ok, [Ans]} = file:consult(AnsAbsFn),
		   P = parse:scan_and_parse_file(CodeAbsFn),
		   P1 = uniquify:uniquify(P),
		   P2 = rco:remove_complex_operands(P1),
		   C = explicate_control:explicate_control(P2),
		   ?assert(C =:= Ans),
		   ok		   
	   end,
    ok = test_utils:test_framework(Func).

-endif.
