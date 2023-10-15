-module(flatten).

-include("common.hrl").

-export([flatten/1]).

-spec flatten(r2_program()) -> c1_program().
flatten({r2_program, {type, Tp}, R2_exp}) ->
    var_server:start(),
    {Sts, {return, V}} = flatten_exp(R2_exp),
    {Vars, {New_sts, {return, New_V}}} = get_vars({Sts, {return, V}}),
    {c1_program,
     {Vars, {type, Tp}},
     New_sts ++ [{return, New_V}]}.

flatten_exp({int, N}) ->
    {[], {return, {int, N}}};
flatten_exp({read}) ->
    Var = var_server:new_var(),
    Sts = [{assign, Var, {read}}],
    {Sts, {return, Var}};
flatten_exp({minus, {int, N}}) ->
    {[], {return, {int, -N}}};
flatten_exp({minus, R1_exp}) ->
    Var = var_server:new_var(),
    {Sts, {return, C1_arg}} = flatten_exp(R1_exp),
    Final_sts = Sts ++ [{assign, Var, {minus, C1_arg}}],
    {Final_sts, {return, Var}};
flatten_exp({plus, {int, N1}, {int, N2}}) ->
    {[], {return, {int, N1+N2}}};
flatten_exp({plus, R1_exp1, R1_exp2}) ->
    Var = var_server:new_var(),
    {Sts1, {return, C1_arg1}} = flatten_exp(R1_exp1),
    {Sts2, {return, C1_arg2}} = flatten_exp(R1_exp2),
    Final_sts = Sts1 ++ Sts2 ++ [{assign, Var, {plus, C1_arg1, C1_arg2}}],
    {Final_sts, {return, Var}};
flatten_exp({var, V}) ->
    {[], {return, V}};
flatten_exp({'let', Vars, Exps, Body}) ->
    Results = lists:map(fun make_bind/1, lists:zip(Vars, Exps)),
    {Sts_body, {return, Arg_body}} = flatten_exp(Body),
    Final_sts = lists:flatten([Sts || {Sts, _} <- Results]) ++ Sts_body,
    {Final_sts, {return, Arg_body}};
flatten_exp({true_exp}) ->
    Res = {bool, c1_true},
    {[], {return, Res}};
flatten_exp({false_exp}) ->
    Res = {bool, c1_false},
    {[], {return, Res}};
flatten_exp({and_exp, Exp1, Exp2}) ->
    If_exp = {if_exp,
              {{cmp, 'eq?'}, {true_exp}, Exp1},
              Exp1,
              Exp2},
    flatten_exp(If_exp);
flatten_exp({not_exp, {true_exp}}) ->
    flatten_exp({false_exp});
flatten_exp({not_exp, {false_exp}}) ->
    flatten_exp({true_exp});
flatten_exp({not_exp, Exp}) ->
    {Sts, {return, Arg}} = flatten_exp(Exp),
    Var = var_server:new_var('not'),
    {Sts ++ [{assign, Var, {not_op, Arg}}],
     {return, Var}};
flatten_exp({{cmp, Cmp}, Exp1, Exp2}) ->
    Var = var_server:new_var(cmp),
    {Sts1, {return, Arg1}} = flatten_exp(Exp1),
    {Sts2, {return, Arg2}} = flatten_exp(Exp2),
    Final_sts = Sts1 ++ Sts2 ++ [{assign, Var, {{cmp, Cmp}, Arg1, Arg2}}],
    {Final_sts, {return, Var}};
flatten_exp({if_exp, Exp1, Exp2, Exp3}) ->
    {Sts1, {return, Arg1}} = flatten_exp(Exp1),
    Var = var_server:new_var('if'),
    {Sts2, {return, Arg2}} = flatten_exp(Exp2),
    {Sts3, {return, Arg3}} = flatten_exp(Exp3),
    If_st = {c1_if,
             {{cmp, 'eq?'}, {bool, c1_true}, Arg1},
             Sts2 ++ [{assign, Var, Arg2}],
             Sts3 ++ [{assign, Var, Arg3}]},
    {Sts1 ++ [If_st], {return, Var}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_bind({Var, Exp}) ->
    {Sts, {return, C_arg}} = flatten_exp(Exp),
    case is_atom(C_arg) of
        true ->
            {[rep_var_in_c1_st(St, C_arg, Var) || St <- Sts], {return, Var}};
        false ->
            {Sts ++ [{assign, Var, C_arg}], {return, Var}}
    end.

rep_var_in_c1_st({assign, V, C1_exp}, V_target, V_want) when V =:= V_target ->
    {assign, V_want, C1_exp};
rep_var_in_c1_st({assign, V, C1_exp}, V_target, V_want) when V /= V_target ->
    {assign, V, rep_var_in_c1_exp(C1_exp, V_target, V_want)}.

rep_var_in_c1_exp({read}, _, _) -> {read};
rep_var_in_c1_exp({minus, Arg1, Arg2}, V_target, V_want) ->
    {minus,
     rep_var_in_c1_arg(Arg1, V_target, V_want),
     rep_var_in_c1_arg(Arg2, V_target, V_want)};
rep_var_in_c1_exp({plus, Arg1, Arg2}, V_target, V_want) ->
    {plus,
     rep_var_in_c1_arg(Arg1, V_target, V_want),
     rep_var_in_c1_arg(Arg2, V_target, V_want)};
rep_var_in_c1_exp(Arg, V_target, V_want) ->
    rep_var_in_c1_arg(Arg, V_target, V_want).

rep_var_in_c1_arg({int, N}, _, _) -> {int, N};
rep_var_in_c1_arg(V, V_target, V_want) when V =:= V_target -> V_want;
rep_var_in_c1_arg(V, V_target, _V_want) when V /= V_target -> V.

%{Vars, {New_sts, {return, New_V}}} = get_vars({Sts, {return, V}}),
get_vars({Sts, {return, V}}) ->
    Raw_vars = sets:to_list(sets:from_list(get_raw_vars(Sts ++ [{return, V}]))),
    Pairs = lists:map(fun get_tuple/1, Raw_vars),
    Dict = group_by_key(Pairs),
    Var_map = build_var_map(dict:to_list(Dict)),
    Vars = [Var || {_, Var} <- dict:to_list(Var_map)],
    {Vars, {[replace_var(St, Var_map) || St <- Sts],
            {return, dict:fetch(V, Var_map)}}}.

get_tuple(V) ->
    [A, B] = string:tokens(atom_to_list(V), "."),
    {A, B}.

group_by_key(Plist) ->
    group_by_key(Plist, dict:new()).

group_by_key([], D) -> D;
group_by_key([{Prefix, N}|Plist], D) ->
    New_d = dict:update(Prefix, fun (L) -> L ++ [N] end, [N], D),
    group_by_key(Plist, New_d).

build_var_map(L) ->
    build_var_map(L, dict:new()).

build_var_map([], D) -> D;
build_var_map([{V, Nlist}|VNlist], D) ->
    New_map = [{make_name(V, Ns),
                make_name(V, I)}
               || {I, Ns} <- lists:zip(lists:seq(1, length(Nlist)),
                                       lists:sort(Nlist))],
    New_dict = dict:from_list(dict:to_list(D) ++ New_map),
    build_var_map(VNlist, New_dict).

make_name(V, N) when is_atom(V) and is_atom(N) ->
    make_name(atom_to_list(V), atom_to_list(N));
make_name(V, N) when is_list(V) and is_list(N) ->
    list_to_atom(string:join([V, N], "."));
make_name(V, N) when is_list(V) and is_integer(N) ->
    list_to_atom(string:join([V, integer_to_list(N)], ".")).

replace_var({assign, V, C1_exp}, Map) ->
    {assign, dict:fetch(V, Map), replace_var_in_exp(C1_exp, Map)};
replace_var({return, C1_arg}, Map) ->
    {return, replace_var_in_arg(C1_arg, Map)};
replace_var({c1_if, Cmp_exp, Sts1, Sts2}, Map) ->
    {c1_if,
     replace_var_in_exp(Cmp_exp, Map),
     [replace_var(St, Map) || St <- Sts1],
     [replace_var(St, Map) || St <- Sts2]}.

replace_var_in_exp({read}, _) -> {read};
replace_var_in_exp({minus, Arg}, Map) -> {minus, replace_var_in_arg(Arg, Map)};
replace_var_in_exp({plus, Arg1, Arg2}, Map) ->
    {plus, replace_var_in_arg(Arg1, Map), replace_var_in_arg(Arg2, Map)};
replace_var_in_exp({not_op, Arg}, Map) ->
    {not_op, replace_var_in_arg(Arg, Map)};
replace_var_in_exp({{cmp, Cmp}, Arg1, Arg2}, Map) ->
    {{cmp, Cmp},
     replace_var_in_arg(Arg1, Map),
     replace_var_in_arg(Arg2, Map)};
replace_var_in_exp(Arg, Map) -> replace_var_in_arg(Arg, Map).

replace_var_in_arg(V, Map) when is_atom(V) ->
    dict:fetch(V, Map);
replace_var_in_arg(Arg, _) -> Arg.


get_raw_vars([]) -> [];
get_raw_vars([{assign, V, Exp}|Sts]) ->
    get_raw_vars_in_exp(Exp) ++ get_raw_vars(Sts) ++ [V];
get_raw_vars([{return, Arg}|Sts]) ->
    get_raw_vars_in_arg(Arg) ++ get_raw_vars(Sts);
get_raw_vars([{c1_if, Cmp_exp, Sts1, Sts2}|Sts]) ->
    lists:append([get_raw_vars_in_exp(Cmp_exp),
                  get_raw_vars(Sts1),
                  get_raw_vars(Sts2),
                  get_raw_vars(Sts)]).

get_raw_vars_in_arg(V) when is_atom(V) -> [V];
get_raw_vars_in_arg(_) -> [].

get_raw_vars_in_exp({read}) -> [];
get_raw_vars_in_exp({minus, Arg}) -> get_raw_vars_in_arg(Arg);
get_raw_vars_in_exp({plus, Arg1, Arg2}) ->
    get_raw_vars_in_arg(Arg1) ++ get_raw_vars_in_arg(Arg2);
get_raw_vars_in_exp({not_op, Arg}) -> get_raw_vars_in_arg(Arg);
get_raw_vars_in_exp({{cmp, _}, Arg1, Arg2}) ->
    get_raw_vars_in_arg(Arg1) ++ get_raw_vars_in_arg(Arg2);
get_raw_vars_in_exp(Arg) -> get_raw_vars_in_arg(Arg).
