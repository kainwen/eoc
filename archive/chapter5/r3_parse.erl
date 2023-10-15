-module(r3_parse).

-include("type_spec.hrl").

-export([parse_file/1, parse_string/1]).


-spec parse([r3_token()]) -> r3_program().
parse(['(', 'program'|Toks]) ->
    {Exp, [')']} = parse_exp(Toks),
    {r3_program, Exp}.

-spec parse_exp([r3_token()]) -> {r3_exp(), [r3_token()]}.
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
    {{'let', Vars, Exps, Body}, R2};
parse_exp(['#t'|Rem_toks]) ->
    {{true_exp}, Rem_toks};
parse_exp(['#f'|Rem_toks]) ->
    {{false_exp}, Rem_toks};
parse_exp(['(', 'and'|Rem_toks]) ->
    {Exp1, R1} = parse_exp(Rem_toks),
    {Exp2, [')'|R2]} = parse_exp(R1),
    {{and_exp, Exp1, Exp2}, R2};
parse_exp(['(', 'not'|Rem_toks]) ->
    {Exp, [')'|R]} = parse_exp(Rem_toks),
    {{not_exp, Exp}, R};
parse_exp(Tks=['(', 'eq?'|_Rem_toks]) ->
    parse_cmp_exp(Tks);
parse_exp(Tks=['(', '<='|_Rem_toks]) ->
    parse_cmp_exp(Tks);
parse_exp(Tks=['(', '>='|_Rem_toks]) ->
    parse_cmp_exp(Tks);
parse_exp(Tks=['(', '>'|_Rem_toks]) ->
    parse_cmp_exp(Tks);
parse_exp(Tks=['(', '<'|_Rem_toks]) ->
    parse_cmp_exp(Tks);
parse_exp(['(', 'if'|Rem_toks]) ->
    {A, R1} = parse_exp(Rem_toks),
    {B, R2} = parse_exp(R1),
    {C, [')'|R3]} = parse_exp(R2),
    {{if_exp, A, B ,C}, R3};
parse_exp(['(', 'void', ')'|Rem_toks]) ->
    {{void}, Rem_toks};
parse_exp(['(', 'vector'|Rem_toks]) ->
    {Exps, [')'|R]} = parse_multiple(fun parse_exp/1, Rem_toks),
    {{tuple_exp, Exps}, R};
parse_exp(['(', 'vector-set!'|Rem_toks]) ->
    {Exp1, R1} = parse_exp(Rem_toks),
    {{int, N}, R2} = parse_exp(R1),
    {Exp2, [')'|R2]} = parse_exp(R2),
    {vector_set_exp, Exp1, N, Exp2};
parse_exp(['(', 'vector-ref'|Rem_toks]) ->
    {Exp, R1} = parse_exp(Rem_toks),
    {{int, N}, [')'|R2]} = parse_exp(R1),
    {{vector_ref_exp, Exp, N}, R2}.

%%%%%%%%%%%% parse internal help functions %%%%%%%%%%%%%%%
-spec parse_cmp_exp([r3_token()]) -> {{{cmp, r3_cmp()}, r3_exp(), r3_exp()}, [r3_token()]}.
parse_cmp_exp(['(', Cmp|Rem_toks]) ->
    {Exp1, R1} = parse_exp(Rem_toks),
    {Exp2, [')'|R2]} = parse_exp(R1),
    {{{cmp, Cmp}, Exp1, Exp2}, R2}.

-spec parse_multiple(fun(([r3_token])-> T), [r3_token()]) -> {[T], [r3_token()]}.
parse_multiple(Fun, Toks) ->
    parse_multiple(Fun, Toks, []).

-spec parse_multiple(fun(([r3_token])-> T), [r3_token()], [T]) -> {[T], [r3_token()]}.
parse_multiple(Fun, Toks, Acc_list) ->
    try Fun(Toks) of
        {Term, R} ->
            parse_multiple(Fun, R, [Term|Acc_list])
    catch
        _:_ -> {lists:reverse(Acc_list), Toks}
    end.

-spec parse_binding([r3_token()]) -> {{atom(), r3_exp()}, [r3_token()]}.
parse_binding(['('|R]) ->
    {{var, V}, R1} = parse_exp(R),
    {Exp, [')'|R2]} = parse_exp(R1),
    {{V, Exp}, R2}.

%% APIs
-spec parse_string(string()) -> r3_program().
parse_string(S) ->
    {ok, Toks, _} = r3_tok:string(S),
    parse(Toks).

-spec parse_file(string()) -> r3_program().
parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    parse_string(Code).
