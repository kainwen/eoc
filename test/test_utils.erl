-ifdef(TEST).
-module(test_utils).

-export([test_framework/1]).

-spec test_framework(fun((string(), string(), string()) -> ok)) -> ok.
test_framework(Func) ->
    {ok, Cwd} = file:get_cwd(),
    ThisFilePath = filename:join([Cwd, ?FILE]),
    TestDirPath = filename:dirname(ThisFilePath),
    CodeDir = filename:join([TestDirPath, "code"]),
    ExpectedDir = filename:join([TestDirPath, "expected"]),
    {ok, CodeFns} = file:list_dir(CodeDir),
    ok = lists:foreach(fun (CodeFn) ->
			       ok = Func(CodeDir, ExpectedDir, CodeFn)
		       end , CodeFns).

-endif.
