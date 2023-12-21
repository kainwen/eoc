-ifdef(TEST).
-module(pe_test).

-include_lib("eunit/include/eunit.hrl").

pe_test() ->
    Func = fun(CodeDir, ExpectedDir, CodeFn) ->
		    CodeAbsFn = filename:join(CodeDir, CodeFn),
		    AnsAbsFn = filename:join(ExpectedDir,
					     string:join([CodeFn, "pe"], ".")),
		    {ok, [Ans]} = file:consult(AnsAbsFn),
		    Prog = parse:scan_and_parse_file(CodeAbsFn),
		    ?assert(partial_eval:pe_prog(Prog) =:= Ans),
		    ok
	    end,
    ok = test_utils:test_framework(Func).

-endif.
