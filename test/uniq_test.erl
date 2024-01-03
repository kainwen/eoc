-ifdef(TEST).
-module(uniq_test).

-include_lib("eunit/include/eunit.hrl").

uniq_test() ->
    Func = fun(CodeDir, ExpectedDir, CodeFn) ->
		    CodeAbsFn = filename:join(CodeDir, CodeFn),
		    AnsAbsFn = filename:join(ExpectedDir,
					     string:join([CodeFn, "uniq"], ".")),
		    {ok, [Ans]} = file:consult(AnsAbsFn),
		    Prog = parse:scan_and_parse_file(CodeAbsFn),
		    ?assert(uniquify:uniquify(Prog) =:= Ans),
		    ok
	    end,
    ok = test_utils:test_framework(Func).

-endif.
