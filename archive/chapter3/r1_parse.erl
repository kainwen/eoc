-module(r1_parse).

-include("common.hrl").

-export([parse_file/1, parse_string/1]).

-spec parse([r1_token()]) -> r1_program().
parse(['(', 'program'|Toks]) ->
    {Exp, [')']} = parse_exp(Toks),
    {r1_program, Exp}.

-spec parse_exp([r1_token()]) -> {r1_exp(), [r1_token()]}.
parse_exp([{integer, N}|Rem_toks]) ->
    {{int, N}, Rem_toks};
parse_exp([{id, Var}|Rem_toks]) ->
    {{var, Var}, Rem_toks};
parse_exp(['(', '-'|Rem_toks]) ->
    {Exp, [')'|R]} = parse_exp(Rem_toks),
    {{minus, Exp}, R};
parse_exp(['(', 'read' ,')'|Rem_toks]) ->
    {{read}, Rem_toks};
parse_exp(['(', '+'|Rem_toks]) ->
    {Exp1, R1} = parse_exp(Rem_toks),
    {Exp2, [')'|R2]} = parse_exp(R1),
    {{plus, Exp1, Exp2}, R2};
parse_exp(['(', 'let', '('|Rem_toks]) ->
    {Bindings, [')'|R1]} = parse_multiple(fun parse_binding/1, Rem_toks),
    {Body, [')'| R2]} = parse_exp(R1),
    Vars = [Var || {Var, _} <- Bindings],
    Exps = [Exp || {_, Exp} <- Bindings],
    {{'let', Vars, Exps, Body}, R2}.

%%%%%%%%%%%% parse internal help functions %%%%%%%%%%%%%%%
parse_multiple(Fun, Toks) ->
    parse_multiple(Fun, Toks, []).

parse_multiple(Fun, Toks, Acc_list) ->
    try Fun(Toks) of
        {Term, R} ->
            parse_multiple(Fun, R, [Term|Acc_list])
    catch
        _:_ -> {lists:reverse(Acc_list), Toks}
    end.

-spec parse_binding([r1_token()]) -> {{atom(), r1_exp()}, [r1_token]}.
parse_binding(['('|R]) ->
    {{var, V}, R1} = parse_exp(R),
    {Exp, [')'|R2]} = parse_exp(R1),
    {{V, Exp}, R2}.

%% APIs
parse_string(S) ->
    {ok, Toks, _} = r1_tok:string(S),
    parse(Toks).

parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    parse_string(Code).
