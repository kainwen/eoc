-module(r0_parse).

-include("common.hrl").

-export([parse/1, parse_file/1]).

-spec parse(string()) -> program().
parse(S) ->
    {ok, Toks, _} = r0_tok:string(S),
    {Exp, []} = parse_exp(Toks),
    {program, Exp}.

-spec parse_exp([token()]) -> {exp(), [token()]}.
parse_exp([{integer, N}|Rem_toks]) ->
    {{int, N}, Rem_toks};
parse_exp(['(', '-'|Rem_toks]) ->
    {Exp, [')'|R]} = parse_exp(Rem_toks),
    {{minus, Exp}, R};
parse_exp(['(', 'read' ,')'|Rem_toks]) ->
    {{read}, Rem_toks};
parse_exp(['(', '+'|Rem_toks]) ->
    {Exp1, R1} = parse_exp(Rem_toks),
    {Exp2, [')'|R2]} = parse_exp(R1),
    {{plus, Exp1, Exp2}, R2}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec parse_file(string()) -> program().
parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    parse(Code).
