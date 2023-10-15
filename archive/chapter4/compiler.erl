-module(compiler).

-export([compile_file/2]).

compile(Code) ->
    R2 = r2_parse:parse_string(Code),
    R2_1 = uniquify:uniquify(R2),
    C1 = flatten:flatten(R2_1),
    X86_star = select_instructions:select_instructions(C1),
    X86_star1 = register_allocation:register_allocation(X86_star),
    X86_star2 = lower_condition:lower_condition(X86_star1),
    Final_x86 = patch_instructions:patch_instructions(X86_star2),
    print_x86:print_x86(Final_x86).

compile_file(Src, Output) ->
    {ok, Data} = file:read_file(Src),
    Code = binary_to_list(Data),
    file:write_file(Output, list_to_binary(compile(Code))).
