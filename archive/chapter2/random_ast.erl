-module(random_ast).

-include("common.hrl").

-export([random_r1_ast/1, generate_random_r1_src/2]).

random_choice(List) ->
    Index = random:uniform(length(List)),
    lists:nth(Index, List).

random_choice_with_weight(Plist) ->
    L = lists:flatten([lists:duplicate(N, T) || {T, N} <- Plist]),
    random_choice(L).

-spec random_r1_ast(integer()) -> r1_program().
random_r1_ast(Max_depth) ->
    {r1_program, random_r1_exp(Max_depth)}.

-spec random_r1_exp(integer()) -> r1_exp().
random_r1_exp(1) -> random_r1_leaf();
random_r1_exp(Max_depth) ->
    r1_combine(random_r1_leaf(),
               random_r1_exp(Max_depth - 1),
               Max_depth-1).

-spec random_r1_leaf() -> r1_exp().
random_r1_leaf() ->
    Choises = [{int, 1}, {read, 1}, {var, 1}],
    Leaf = case random_choice_with_weight(Choises) of
               int  -> {int, random:uniform(255)};
               read -> {read};
               var -> {var, random_choice([a, b, c, d])}
           end,
    case random_choice([0, 1, 2]) of
        0 -> {minus, Leaf};
        _ -> Leaf
    end.

random_bind_exp(Max_depth) ->
    Exp = random_r1_exp(Max_depth),
    case free_var(Exp) of
        [] -> Exp;
        _ -> random_bind_exp(Max_depth)
    end.

-spec r1_combine(r1_exp(), r1_exp(), integer()) -> r1_exp().
r1_combine(R1_exp1, R1_exp2, Max_depth) ->
    Fvars = sets:to_list(sets:from_list(free_var(R1_exp1) ++ free_var(R1_exp2))),
    case length(Fvars) of
        0 -> {plus, R1_exp1, R1_exp2};
        _ ->
            Exps = [random_bind_exp(Max_depth) || _ <- Fvars],
            {'let', Fvars, Exps, {plus, R1_exp1, R1_exp2}}
    end.

-spec free_var(r1_exp()) -> [atom()].
free_var({int, _}) -> [];
free_var({read}) -> [];
free_var({minus, E}) -> free_var(E);
free_var({plus, E1, E2}) ->
    sets:to_list(sets:from_list(free_var(E1) ++ free_var(E2)));
free_var({var, V}) -> [V];
free_var({'let', Vars, Exps, Body}) ->
    Fvs = sets:from_list(lists:flatten([free_var(Exp) || Exp <- Exps])),
    Fvs_body = sets:subtract(sets:from_list(free_var(Body)),
                             sets:from_list(Vars)),
    sets:to_list(sets:union(Fvs, Fvs_body)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_random_r1_src(0, _) -> ok;
generate_random_r1_src(N, Dir) ->
    Max_depth = random_choice([3,4]),
    Src = ast_to_src:to_r1(random_r1_ast(Max_depth)),
    Code_file = filename:join(Dir,
                              string:join([integer_to_list(N), "ss"], ".")),
    file:write_file(Code_file, io_lib:format("~s", [Src])),
    generate_random_r1_src(N-1, Dir).
