-module(test).

-export([test/0, test_with_pe/0]).

test() ->
    gen_x86_code(),
    compile_x86_with_gcc(),
    check(run_exe()).

test_with_pe() ->
    gen_x86_code_with_pe(),
    compile_x86_with_gcc(),
    check(run_exe()).

gen_x86_code() ->
    Code_dir = "test/code",
    {ok, Fns} = file:list_dir(Code_dir),
    [gen_x86_code(filename:join(Code_dir, Fn), Fn)
     || Fn <- Fns].

gen_x86_code(Code_path, Out_fn) ->
    compiler:compile_file(Code_path,
                          filename:join("test/x86_code", Out_fn ++ ".s")).

gen_x86_code_with_pe() ->
    Code_dir = "test/code",
    {ok, Fns} = file:list_dir(Code_dir),
    [gen_x86_code_with_pe(filename:join(Code_dir, Fn), Fn)
     || Fn <- Fns].

gen_x86_code_with_pe(Code_path, Out_fn) ->
    compiler:compile_file_with_pe(Code_path,
                                  filename:join("test/x86_code", Out_fn ++ ".s")).

compile_x86_with_gcc() ->
    X86_code_dir = "test/x86_code",
    {ok, Fns} = file:list_dir(X86_code_dir),
    [compile_x86_with_gcc(filename:join(X86_code_dir, Fn), Fn ++ ".exe")
     || Fn <- Fns].

compile_x86_with_gcc(Code_path, Exe_path) ->
    Cmd = "gcc test/runtime.c " ++ Code_path ++ " -I test/ -o " ++ Exe_path,
    os:cmd(Cmd).

run_exe() ->
    X86_code_dir = "test/x86_code",
    {ok, Fns} = file:list_dir(X86_code_dir),
    {ok, Cwd} = file:get_cwd(),
    All_exes = [filename:join(Cwd, Fn ++ ".exe") || Fn <- Fns],
    Inputs = [filename:join("test/input/", string:sub_string(Fn, 1, 4) ++ ".in")
              || Fn <- Fns],
    [{Input, run_exe(Exe, Input)} || {Exe, Input} <- lists:zip(All_exes, Inputs)].

run_exe(Exe, Input) ->
    Out = os:cmd(Exe ++ " < " ++ Input),
    os:cmd("rm " ++ Exe),
    Out.

check(Exe_out_list) ->
    Std_files = [filename:join("test/out1",
                          string:sub_string(filename:basename(Input), 1, 1))
            ||{Input, _Ans} <- Exe_out_list],
    Stds = [binary_to_list(element(2, file:read_file(Std))) || Std <- Std_files],
    Exe_outs = [Ans || {_, Ans} <- Exe_out_list],
    [list_to_integer(string:strip(A, right, $\n))
     =:= list_to_integer(string:strip(B, right, $\n))
     || {A, B} <- lists:zip(Stds, Exe_outs)].
