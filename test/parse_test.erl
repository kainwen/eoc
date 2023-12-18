-ifdef(TEST).
-module(parse_test).

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    {ok, Cwd} = file:get_cwd(),
    ThisFilePath = filename:join([Cwd, ?FILE]),
    TestDirPath = filename:dirname(ThisFilePath),
    CodeDir = filename:join([TestDirPath, "code"]),
    ExpectedDir = filename:join([TestDirPath, "expected"]),
    {ok, CodeFns} = file:list_dir(CodeDir),
    ok = lists:foreach(fun (CodeFn) ->
			       CodeAbsFn = filename:join(CodeDir, CodeFn),
			       AnsAbsFn = filename:join(ExpectedDir, CodeFn),
			       {ok, [Ans]} = file:consult(AnsAbsFn),
			       ?assert(parse:scan_and_parse_file(CodeAbsFn) =:= Ans),
			       ok
		       end,
		       CodeFns).

-endif.
