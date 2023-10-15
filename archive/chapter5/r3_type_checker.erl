-module(r3_type_checker).

-include("type_spec.hrl").

-export([typer/1]).

-spec typer(r3_ext_program()) -> r3_tp_program().
typer({r3_ext_program, Exp}) ->
    E = {has_type, _, Tp} = typer(Exp, []),
    {r3_tp_program, {type, Tp}, E}.

-spec typer(r3_ext_exp(), [{atom(), r3_tp_type()}]) -> r3_tp_exp().
typer(Exp={int, _}, _) -> {has_type, Exp, int};
typer(Exp={read}, _) -> {has_type, Exp, int};
typer({minus, Exp}, Tenv) ->
    E = {has_type, _, int} = typer(Exp, Tenv),
    {has_type, {minus, E}, int};
typer({plus, Exp1, Exp2}, Tenv) ->
    E1 = {has_type, _, int} = typer(Exp1, Tenv),
    E2 = {has_type, _, int} = typer(Exp2, Tenv),
    {has_type, {plus, E1, E2}, int};
typer({var, V}, Tenv) ->
    {has_type, {var, V}, proplists:get_value(V, Tenv)};
typer({'let', Vars, Exps, Body}, Tenv) ->
    Exp_results = [typer(Exp, Tenv) || Exp <- Exps],
    New_var_tps = [{Var, Tp}
                   || {Var, {has_type, _, Tp}} <- lists:zip(Vars, Exp_results)],
    New_tenv = New_var_tps ++ Tenv,
    {has_type, _, Tp_body} = Body_result = typer(Body, New_tenv),
    {has_type,
     {'let',
      Vars,
      Exp_results,
      Body_result},
     Tp_body};
typer(Exp={true_exp}, _Tenv) -> {has_type, Exp, bool};
typer(Exp={false_exp}, _Tenv) -> {has_type, Exp, bool};
typer({and_exp, Exp1, Exp2}, Tenv) ->
    E1 = {has_type, _, bool} = typer(Exp1, Tenv),
    E2 = {has_type, _, bool} = typer(Exp2, Tenv),
    {has_type, {and_exp, E1, E2}, bool};
typer({not_exp, Exp}, Tenv) ->
    E = {has_type, _, int} = typer(Exp, Tenv),
    {has_type, {not_exp, E}, bool};
typer({{cmp, Cmp}, Exp1, Exp2}, Tenv) ->
    E1 = {has_type, _, int} = typer(Exp1, Tenv),
    E2 = {has_type, _, int} = typer(Exp2, Tenv),
    {has_type, {{cmp, Cmp}, E1, E2}, bool};
typer({if_exp, Exp1, Exp2, Exp3}, Tenv) ->
    E1 = {has_type, _, bool} = typer(Exp1, Tenv),
    E2 = {has_type, _, T} = typer(Exp2, Tenv),
    E3 = {has_type, _, T} = typer(Exp3, Tenv),
    {has_type,
    {if_exp, E1, E2, E3},
     T};
typer({void}, _) -> {has_type, {void}, void};
typer({vector_set_exp, Exp1, N, Exp2}, Tenv) ->
    E1 = {has_type, _, {tuple, Tps}} = typer(Exp1, Tenv),
    E2 = {has_type, _, T} = typer(Exp2, Tenv),
    T = lists:nth(N+1, Tps),
    {has_type,
     {vector_set_exp,
      E1,
      N,
      E2},
     void};
typer({vector_ref_exp, Exp, N}, Tenv) ->
    E = {has_type, _, {tuple, Tps}} = typer(Exp, Tenv),
    T = lists:nth(N+1, Tps),
    {has_type,
     {vector_ref_exp, E, N},
     T};
typer(Exp={collect, _}, _) -> {has_type, Exp, void};
typer({allocate, N, place_holder}, [_|Tenv]) ->
    %% trick here: Tenv = [dummy, x0 x1 ... xn-1 | _]
    T = {tuple, [Tp || {_V, Tp} <- lists:sublist(Tenv, N)]},
    {has_type, {allocate, N, T}, T};
typer(Exp={global_value, _}, _Tenv) -> {has_type, Exp, int}.
