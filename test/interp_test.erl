-ifdef(TEST).
-module(interp_test).

-include_lib("eunit/include/eunit.hrl").

interp_test() ->
    ok = meck:new(utils),
    ok = meck:expect(utils, read_from_stdin, fun() -> 5 end),

    Func1 = fun(CodeDir, ExpectedDir, CodeFn) ->
		    CodeAbsFn = filename:join(CodeDir, CodeFn),
		    AnsAbsFn = filename:join(ExpectedDir,
					     string:join([CodeFn, "interp"], ".")),
		    {ok, [Ans]} = file:consult(AnsAbsFn),
		    Prog = parse:scan_and_parse_file(CodeAbsFn),
		    ?assert(interpreter:interp(Prog) =:= Ans),
		    ok
	    end,

    ok = test_utils:test_framework(Func1),

    Func2 = fun(CodeDir, ExpectedDir, CodeFn) ->
		    CodeAbsFn = filename:join(CodeDir, CodeFn),
		    AnsAbsFn = filename:join(ExpectedDir,
					     string:join([CodeFn, "interp"], ".")),
		    {ok, [Ans]} = file:consult(AnsAbsFn),
		    Prog = parse:scan_and_parse_file(CodeAbsFn),
		    Prog1 = uniquify:uniquify(Prog),
		    ?assert(interpreter:interp(Prog1) =:= Ans),
		    ok
	    end,

    ok = test_utils:test_framework(Func2),
    
    Func3 = fun(CodeDir, ExpectedDir, CodeFn) ->
		    CodeAbsFn = filename:join(CodeDir, CodeFn),
		    AnsAbsFn = filename:join(ExpectedDir,
					     string:join([CodeFn, "interp"], ".")),
		    {ok, [Ans]} = file:consult(AnsAbsFn),
		    Prog = parse:scan_and_parse_file(CodeAbsFn),
		    Prog1 = rco:remove_complex_operands(Prog),
		    ?assert(interpreter:interp(Prog1) =:= Ans),
		    ok
	    end,
    
    ok = test_utils:test_framework(Func3),
    
    Func4 = fun(CodeDir, _ExpectedDir, CodeFn) ->
		    CodeAbsFn = filename:join(CodeDir, CodeFn),
		    Prog = parse:scan_and_parse_file(CodeAbsFn),
		    Prog1 = uniquify:uniquify(Prog),
		    Prog2 = rco:remove_complex_operands(Prog1),
		    Prog3 = explicate_control:explicate_control(Prog2),
		    V = interpreter:interp(Prog),
		    V1 = interpreter:interp(Prog1),
		    V2 = interpreter:interp(Prog2),
		    V3 = cvar_interp:interp(Prog3),
		    ?assert(V1 =:= V),
		    ?assert(V2 =:= V),
		    ?assert(V3 =:= V),
		    ok
	    end,
    
    ok = test_utils:test_framework(Func4).

-endif.
