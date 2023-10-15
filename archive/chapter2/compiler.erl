-module(compiler).

-export([compile/1, compile_file/2, compile_file_with_pe/2]).

-spec compile(string()) -> string().
compile(Code) ->
    R1 = r1_parse:parse_string(Code),
    R1_1 = uniquify:uniquify(R1),
    C0 = flatten:flatten(R1_1),
    X86_star = select_instructions:select_instructions(C0),
    X86 = assign_homes:assign_homes(X86_star),
    Final_x86 = patch_instructions:patch_instructions(X86),
    print_x86:print_x86(Final_x86).

compile_file(Src, Output) ->
    R1 = r1_parse:parse_file(Src),
    R1_1 = uniquify:uniquify(R1),
    C0 = flatten:flatten(R1_1),
    X86_star = select_instructions:select_instructions(C0),
    X86 = assign_homes:assign_homes(X86_star),
    Final_x86 = patch_instructions:patch_instructions(X86),
    Assembly_code = print_x86:print_x86(Final_x86),
    file:write_file(Output, list_to_binary(Assembly_code)).

compile_file_with_pe(Src, Output) ->
    R1 = r1_parse:parse_file(Src),
    R1_1 = uniquify:uniquify(R1),
    R1_2 = pe:pe(R1_1),
    C0 = flatten:flatten(R1_2),
    X86_star = select_instructions:select_instructions(C0),
    X86 = assign_homes:assign_homes(X86_star),
    Final_x86 = patch_instructions:patch_instructions(X86),
    Assembly_code = print_x86:print_x86(Final_x86),
    file:write_file(Output, list_to_binary(Assembly_code)).
