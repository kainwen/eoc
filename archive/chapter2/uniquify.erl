-module(uniquify).

-include("common.hrl").

-export([uniquify/1, uniquify_dir/2]).

-spec uniquify(r1_program()) -> r1_program().
uniquify({r1_program, Exp}) ->
    name_server:start(),
    {r1_program, uniquify(Exp, senv:empty_senv())}.

-spec uniquify(r1_exp(), senv:senv()) -> r1_exp().
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
    {'let', Renamed_vars, Renamed_exps, Renamed_body}.

uniquify_dir(Dir, OutDir) ->
    {ok, Fns} = file:list_dir(Dir),
    [uniquify_and_gen_src(Dir, Fn, OutDir) || Fn <- Fns].

uniquify_and_gen_src(Dir, Fn, OutDir) ->
    Code_path = filename:join(Dir, Fn),
    Ast = r1_parse:parse_file(Code_path),
    Code = ast_to_src:to_r1(Ast),
    Out_file = filename:join(OutDir, Fn),
    file:write_file(Out_file, io_lib:format("~s", [Code])).

%% Internal help functions
-spec rename(atom(), integer()) -> atom().
rename(V, Id) ->
    S_v = atom_to_list(V),
    S_id = integer_to_list(Id),
    S = string:join([S_v, S_id], "."),
    list_to_atom(S).
