-module(pe).

-include("common.hrl").

-export([pe/1]).

%% the input r1 language has been renamed.

-type sign() :: 1 | -1.

-type index() :: integer().

-type partial_value() :: {integer(), [{sign(), index()}]}.

-type pe_env() :: [{atom(), partial_value()}].

pe({r1_program, Exp}) ->
    read_index:start(),
    {N, Read_list} = eval_to_pv(Exp, []),
    Ordered_read_list = lists:sort(fun ({_, A}, {_, B}) -> A < B end,
                                   Read_list),
    Rs = [S || {S, _I} <- Ordered_read_list],
    {r1_program, build_r1({N, Rs})}.

-spec eval_to_pv(r1_exp(), pe_env()) -> partial_value().
eval_to_pv({int, N}, _Pe_env) -> {N, []};
eval_to_pv({read}, _Pe_env) -> {0, [{1, read_index:get_index()}]};
eval_to_pv({minus, Exp}, Pe_env) ->
    {N, Read_list} = eval_to_pv(Exp, Pe_env),
    {-N, [ {-1 * R, I} || {R, I} <- Read_list]};
eval_to_pv({plus, Exp1, Exp2}, Pe_env) ->
    {N1, Rs1} = eval_to_pv(Exp1, Pe_env),
    {N2, Rs2} = eval_to_pv(Exp2, Pe_env),
    {N1+N2, Rs1 ++ Rs2};
eval_to_pv({var, V}, Pe_env) ->
    case proplists:get_value(V, Pe_env) of
        undefined ->
            erlang:error({can_not_find_var, V});
        Pv -> Pv
    end;
eval_to_pv({'let', Vars, Exps, Body}, Pe_env) ->
    Vals = [eval_to_pv(Exp, Pe_env) || Exp <- Exps],
    New_pe_env = lists:zip(Vars, Vals) ++ Pe_env,
    eval_to_pv(Body, New_pe_env).

build_r1({N, []}) ->
    {int, N};
build_r1({0, Rs}) ->
    combine_read(Rs);
build_r1({N, Rs}) ->
    {plus, {int, N}, combine_read(Rs)}.

-spec combine_read([sign()]) -> r1_exp().
combine_read([1]) -> {read};
combine_read([-1]) -> {minus, {read}};
combine_read([R|Rs]) ->
    Result1 = combine_read([R]),
    Result2 = combine_read(Rs),
    {plus, Result1, Result2}.
