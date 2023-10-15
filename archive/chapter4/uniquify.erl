-module(uniquify).

-include("common.hrl").

-export([uniquify/1]).

-spec uniquify(r2_program()) -> r2_program().
uniquify({r2_program, {type, Tp}, Exp}) ->
    name_server:start(),
    {r2_program, {type, Tp}, uniquify(Exp, senv:empty_senv())}.

-spec uniquify(r2_exp(), senv:senv()) -> r2_exp().
uniquify({int, N}, _Senv) -> {int, N};
uniquify({read}, _Senv) -> {read};
uniquify({minus, Exp}, Senv) ->
    {minus, uniquify(Exp, Senv)};
uniquify({plus, Exp1, Exp2}, Senv) ->
    {plus, uniquify(Exp1, Senv), uniquify(Exp2, Senv)};
uniquify({var, V}, Senv) ->
    Id = senv:apply_senv(Senv, V),
    {var, rename(V, Id)};
uniquify({'let', Vars, Exps, Body}, Senv) ->
    Renamed_exps = [uniquify(Exp, Senv) || Exp <- Exps],
    New_senv = senv:extend_senv(Senv, Vars),
    Renamed_body = uniquify(Body, New_senv),
    Renamed_vars = [rename(Var, senv:apply_senv(New_senv, Var))
                    || Var <- Vars],
    {'let', Renamed_vars, Renamed_exps, Renamed_body};
uniquify({true_exp}, _) -> {true_exp};
uniquify({false_exp}, _) -> {false_exp};
uniquify({and_exp, Exp1, Exp2}, Senv) ->
    {and_exp, uniquify(Exp1, Senv), uniquify(Exp2, Senv)};
uniquify({not_exp, Exp}, Senv) ->
    {not_exp, uniquify(Exp, Senv)};
uniquify({{cmp, Cmp}, Exp1, Exp2}, Senv) ->
    {{cmp, Cmp}, uniquify(Exp1, Senv), uniquify(Exp2, Senv)};
uniquify({if_exp, Exp1, Exp2, Exp3}, Senv) ->
    {if_exp, uniquify(Exp1, Senv), uniquify(Exp2, Senv), uniquify(Exp3, Senv)}.

%% Internal help functions
-spec rename(atom(), integer()) -> atom().
rename(V, Id) ->
    S_v = atom_to_list(V),
    S_id = integer_to_list(Id),
    S = string:join([S_v, S_id], "."),
    list_to_atom(S).
