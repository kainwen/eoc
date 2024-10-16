-module(parse).

-export([scan_and_parse/1, scan_and_parse_file/1]).

-include("ast.hrl").

-spec scan_and_parse(string()) -> program().
scan_and_parse(Code) ->
    {ok, Toks, _} = toks:string(Code),
    {ok, Prog} = grammar:parse(Toks),
    Prog.

-spec scan_and_parse_file(string()) -> program().
scan_and_parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    scan_and_parse(Code).


