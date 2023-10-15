-module(allocate_registers).

-include("common.hrl").

-export([allocate_registers/1]).

-define(INT_SIZE, 8).

-spec allocate_registers(x86_64_gamma_program()) -> x86_64_mu_program().
allocate_registers({x86_64_gamma_program, {Vars, Graph, _Reg_vars}, Insts}) ->
    Color_result = dict:to_list(color_graph(Vars, Graph)),
    Color_reg_map = dict:from_list([
                                    {0, {register, r12}},
                                    {1, {register, r13}},
                                    {2, {register, r14}},
                                    {3, {register, r15}},
                                    {4, {register, rbx}}
                                   ]),
    Reg_alloc_result = [{V, get_reg(C, Color_reg_map)} || {V, C} <- Color_result],
    {x86_64_mu_program, Reg_alloc_result, Insts}.

-spec color_graph([x86_64_star_var()], digraph:graph()) -> dict:dict().
color_graph(Vars, Graph) ->
    Color_result = dict:new(),
    Ban_colors = dict:from_list([{Var, sets:new()}|| Var <- Vars]),
    loop(Ban_colors, Color_result, Graph).

loop(Banned_colors, Color_result, Graph) ->
    case dict:size(Banned_colors) of
        N  when N > 0 ->
            {{V, S}, New_banned_colors} = pop_max(Banned_colors),
            Color = get_color(S),
            New_color_result = dict:store(V, Color, Color_result),
            Es = [digraph:edge(Graph, E) || E <- digraph:edges(Graph, V)],
            Vs = [V2 || {_, V1, V2, _} <- Es, V1 =:= V],
            loop(update_banned_colors(New_banned_colors, Vs, Color),
                 New_color_result,
                 Graph);
        0 -> Color_result
    end.

%%%%%%%%%%%%%%%Internal Functions%%%%%%%%%%%%%%%%%%
update_banned_colors(Banned_colors, [], _Color) -> Banned_colors;
update_banned_colors(Banned_colors, [V|Vs], Color) ->
    case dict:is_key(V, Banned_colors) of
        true ->
            New_banned_colors = dict:update(V,
                                            fun (S) ->
                                                    sets:add_element(Color, S)
                                            end,
                                            Banned_colors),
            update_banned_colors(New_banned_colors, Vs, Color);
        false ->
            update_banned_colors(Banned_colors, Vs, Color)
    end.


pop_max(Banned_colors) ->
    L = dict:to_list(Banned_colors),
    Rl = lists:sort(fun ({_V1, S1}, {_V2, S2}) ->
                            sets:size(S1) > sets:size(S2)
                    end,
                    L),
    [{V,S}|R] = Rl,
    {{V, S}, dict:from_list(R)}.

get_color(S) -> get_color(S, 0).

get_color(S, N) ->
    case sets:is_element(N, S) of
        true ->
            get_color(S, N+1);
        false ->
            N
    end.

get_reg(C, Color_reg_map) ->
    case dict:is_key(C, Color_reg_map) of
        true ->
            dict:fetch(C, Color_reg_map);
        false ->
            Off = C - dict:size(Color_reg_map),
            -(Off * ?INT_SIZE)
    end.
