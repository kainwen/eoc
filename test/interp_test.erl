-ifdef(TEST).
-module(interp_test).

-include_lib("eunit/include/eunit.hrl").

interp_test() ->
    ok = meck:new(utils),
    ok = meck:expect(utils, read_from_stdin, fun() -> 5 end),

    Func = fun(CodeDir, ExpectedDir, CodeFn) ->
		    CodeAbsFn = filename:join(CodeDir, CodeFn),
		    AnsAbsFn = filename:join(ExpectedDir,
					     string:join([CodeFn, "interp"], ".")),
		    {ok, [Ans]} = file:consult(AnsAbsFn),
		    Prog = parse:scan_and_parse_file(CodeAbsFn),
		    ?assert(interpreter:interp(Prog) =:= Ans),
		    ok
	    end,

    ok = test_utils:test_framework(Func).

-endif.