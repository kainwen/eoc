-module(flatten).

-include("type_spec.hrl").

%-export([flatten/1]).
-compile(export_all).

-spec flatten(r3_tp_program()) -> c2_program().
flatten({r3_tp_program, {type, Tp}, Exp}) ->
    var_server:start(),
    {Sts, {return, Arg}} = flatten_exp(Exp),
    Vars = var_server:get(),
    {c2_program, Vars, {type, Tp}, Sts ++ [{return, Arg}]}.

-spec flatten_exp(r3_tp_exp()) -> {[c2_statement()], {return, c2_arg()}}.
flatten_exp({has_type, {int, N}, _}) ->
    {[], {return, {int, N}}};
flatten_exp({has_type, {read}, int}) ->
    Var = var_server:new(),
    var_server:add([{Var, int}]),
    {[{assign, Var, {read}}], {return, Var}};
flatten_exp({has_type, {minus, Exp}, int}) ->
    case Exp of
        {int, N} -> flatten_exp({has_type, {int, -N}, int});
        _ ->
            {Sts, {return, Arg}} = flatten_exp(Exp),
            Var = var_server:new(),
            var_server:add([{Var, int}]),
            New_sts = Sts ++ [{assign, Var, {minus, Arg}}],
            {New_sts, {return, Var}}
    end;
flatten_exp({has_type, {plus, Exp1, Exp2}, int}) ->
    case {Exp1, Exp2} of
        {{has_type, {int, N1}, int}, {has_type, {int, N2}, int}} ->
            flatten_exp({has_type, {int, N1 + N2}, int});
        {{has_type, {int, N}, int}, Exp} ->
            {Sts, {return, Arg}} = flatten_exp(Exp),
            Var = var_server:new(),
            var_server:add([{Var, int}]),
            New_sts = Sts ++ [{assign, Var, {plus, {int, N}, Arg}}],
            {New_sts, {return, Var}};
        {_, {has_type, {int, _}, int}} ->
            flatten_exp({has_type, {plus, Exp2, Exp1}, int});
        _ ->
            {Sts1, {return, Arg1}} = flatten_exp(Exp1),
            {Sts2, {return, Arg2}} = flatten_exp(Exp2),
            Var = var_server:new(),
            var_server:add([{Var, int}]),
            New_sts = Sts1 ++ Sts2 ++ [{assign, Var, {plus, Arg1, Arg2}}],
            {New_sts, {return, Var}}
    end;
flatten_exp({has_type, {var, V}, _}) ->
    {[], {return, V}};
flatten_exp({has_type, {'let', Vars, Exps, Body}, _}) ->
    loop(lists:zip(Vars, Exps), Body, []);
flatten_exp({has_type, {true_exp}, bool}) ->
    {[], {return, {bool, c2_true}}};
flatten_exp({has_type, {false_exp}, bool}) ->
    {[], {return, {bool, c2_false}}};
flatten_exp({has_type, {and_exp, Exp1, Exp2}, bool}) ->
    case {Exp1, Exp2} of
        {{has_type, {false_exp}, bool}, _} -> flatten_exp(Exp1);
        {_, {has_type, {false_exp}, bool}} -> flatten_exp(Exp2);
        {{has_type, {true_exp}, bool}, _} -> flatten_exp(Exp2);
        {_, {has_type, {true_exp}, bool}} -> flatten_exp(Exp1);
        _ ->
            Cmp1 = {has_type, {true_exp}, bool},
            Cmp_exp = {has_type, {{cmp, 'eq?'}, Cmp1, Exp1}, bool},
            If_exp = {has_type,
                      {if_exp,
                       Cmp_exp,
                       Exp2,
                       {has_type, {false_exp}, bool}},
                      bool},
            flatten_exp(If_exp)
    end;
flatten_exp({has_type, {not_exp, Exp}, bool}) ->
    case Exp of
        {has_type, {true_exp}, bool} ->
            flatten_exp({has_type, {false_exp}, bool});
        {has_type, {false_exp}, bool} ->
            flatten_exp({has_type, {true_exp}, bool});
        _ ->
            {Sts, {return, Arg}} = flatten_exp(Exp),
            Var = var_server:new(),
            var_server:add([{Var, bool}]),
            New_sts = Sts ++ [{assign, Var, Arg}],
            {New_sts, {return, Var}}
    end;
flatten_exp({has_type, {{cmp, Cmp}, Exp1, Exp2}, bool}) ->
    {Sts1, {return, Arg1}} = flatten_exp(Exp1),
    {Sts2, {return, Arg2}} = flatten_exp(Exp2),
    Var = var_server:new(),
    var_server:add([{Var, bool}]),
    New_sts = Sts1 ++ Sts2 ++ [{assign, Var, {{cmp, Cmp}, Arg1, Arg2}}],
    {New_sts, {return, Var}};
flatten_exp({has_type, {if_exp, Exp1, Exp2, Exp3}, Type}) ->
    {Sts1, {return, Arg1}} = flatten_exp(Exp1),
    Var = var_server:new('if'),
    var_server:add([{Var, Type}]),
    {Sts2, {return, Arg2}} = flatten_exp(Exp2),
    {Sts3, {return, Arg3}} = flatten_exp(Exp3),
    If_st = {c2_if,
             {{cmp, 'eq?'}, {bool, c2_true}, Arg1},
             Sts2 ++ [{assign, Var, Arg2}],
             Sts3 ++ [{assign, Var, Arg3}]},
    {Sts1 ++ [If_st], {return, Var}};
flatten_exp({has_type, {void}, void}) ->
    {[], {return, {void}}};
flatten_exp({has_type, {vector_set_exp, Exp1, N, Exp2}, void}) ->
    {Sts1, {return, Arg1}} = flatten_exp(Exp1),
    {Sts2, {return, Arg2}} = flatten_exp(Exp2),
    Var = var_server:new(),
    var_server:add([{Var, void}]),
    New_sts = Sts1 ++ Sts2 ++ [{assign, Var, {vector_set, Arg1, N, Arg2}}],
    {New_sts, {return, Var}};
flatten_exp({has_type, {vector_ref_exp, Exp, N}, Type}) ->
    {Sts, {return, Arg}} = flatten_exp(Exp),
    Var = var_server:new(),
    var_server:add([{Var, Type}]),
    New_sts = Sts ++ [{assign, Var, {vector_ref, Arg, N}}],
    {New_sts, {return, Var}};
flatten_exp({has_type, {collect, N}, void}) ->
    Sts = [{collect, N}],
    {Sts, {return, {void}}};
flatten_exp({has_type, {allocate, N, Type}, Rtp}) ->
    Var = var_server:new(),
    var_server:add([{Var, Rtp}]),
    {[{assign, Var, {allocate, N, Type}}], {return, Var}};
flatten_exp({has_type, {global_value, Name}, Type}) ->
    Var = var_server:new(),
    var_server:add([{Var, Type}]),
    {[{assign, Var, {global_value, Name}}], {return, Var}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop([], Body, Sts) ->
    {Sts_body, {return, Arg_body}} = flatten_exp(Body),
    Final_sts = Sts ++ Sts_body,
    {Final_sts, {return, Arg_body}};
loop([{Var, Exp}|Var_exp_list], Body, Sts) ->
    {Sts_exp, {return, Arg}} = flatten_exp(Exp),
    case Arg of
        V when is_atom(V) ->
            New_body = replace_var_in_exp(Body, {var, Var}, {var, V}),
            loop(Var_exp_list, New_body, Sts ++ Sts_exp);
        _ ->
            {has_type, _, T} = Exp,
            var_server:add([{Var, T}]),
            loop(Var_exp_list, Body, Sts ++ Sts_exp ++ [{assign, Var, Arg}])
    end.

replace_var_in_exp(V, _, _)  when is_atom(V) -> V;
replace_var_in_exp(N, _, _)  when is_integer(N) -> N;
replace_var_in_exp(L, Old, New) when is_list(L) ->
    [replace_var_in_exp(E, Old, New) || E <- L];
replace_var_in_exp({var, V}, {var, V}, New) -> New;
replace_var_in_exp({var, Var}, {var, V}, _New) when Var /= V -> {var, Var};
replace_var_in_exp(Exp, Old, New) ->
    Ts = tuple_size(Exp),
    list_to_tuple([replace_var_in_exp(element(I, Exp), Old, New)
                   || I <- lists:seq(1, Ts)]).
