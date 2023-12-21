-ifdef(TEST).
-module(parse_test).

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    Func = fun (CodeDir, ExpectedDir, CodeFn) ->
		   CodeAbsFn = filename:join(CodeDir, CodeFn),
		   AnsAbsFn = filename:join(ExpectedDir,
					    string:join([CodeFn, "parse"], ".")),
		   {ok, [Ans]} = file:consult(AnsAbsFn),
		   ?assert(parse:scan_and_parse_file(CodeAbsFn) =:= Ans),
		   ok		   
	   end,
    ok = test_utils:test_framework(Func).

-endif.
