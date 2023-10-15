-module(register_allocation).

-include("common.hrl").

%-export([register_allocation/1]).
-compile(export_all).

-define(INT_SIZE, 8).

-spec register_allocation(x86_star_program()) -> x86_star_program().
register_allocation({x86_star_program, Vars, {type, Tp}, Insts}) ->
    {Code, Lvl} = uncover_liveness(Insts),
    {G, S, New_code} = build_interference(Vars, {Code, Lvl}),
    Var_reg_map = allocate_registers(Vars, G, S),
    {Frame_size, Final_code} = assign_homes(Var_reg_map, New_code),
    {x86_star_program, [{frame_size, Frame_size}], {type, Tp}, Final_code}.

%%%%%%%%%%%%%%%%%%%liveness analysis and helper functions%%%%%%%%%%%%%%%%%%%%%%
-spec uncover_liveness([x86_star_instruction()])
                      -> {[x86_star_instruction()], [[x86_star_var()]]}.
uncover_liveness(Insts) ->
    find_lives(lists:reverse(Insts), [[]], []).

find_lives([I], Live_vars_list, Code) -> {[I|Code], Live_vars_list};
find_lives([{c1_if, {{cmp, Cmp}, Arg1, Arg2}, Insts1, Insts2}|Insts],
           Lvl=[Live_vars|_Live_vars_list],
           Code) ->
    {Code_then, Lvl_then} = find_lives(lists:reverse(Insts1) ++ [{retq}],
                                       [Live_vars],
                                       []),
    {Code_else, Lvl_else} = find_lives(lists:reverse(Insts2) ++ [{retq}],
                                       [Live_vars],
                                       []),
    New_lvs = reduce_dup(lists:append([lists:nth(1, Lvl_then),
                                       lists:nth(1, Lvl_else),
                                       find_vars_in_arg(Arg1),
                                       find_vars_in_arg(Arg2)])),
    New_code = [{c1_if,
                 {{cmp, Cmp}, Arg1, Arg2},
                 {lists:sublist(Code_then, 2, length(Code_then)),
                  lists:sublist(Lvl_then, 2, length(Lvl_then))},
                 {lists:sublist(Code_else, 2, length(Code_else)),
                  lists:sublist(Lvl_else, 2, length(Lvl_else))}} |
                Code],
    find_lives(Insts, [New_lvs|Lvl], New_code);
find_lives([Inst|Insts], Lvl=[Live_vars|_Live_vars_list], Code) ->
    Write_vars = get_write_var(Inst),
    Read_vars = get_read_vars(Inst),
    Sw = sets:from_list(Write_vars),
    Sr = sets:from_list(Read_vars),
    Slv = sets:from_list(Live_vars),
    S_new_live_vars = sets:union(sets:subtract(Slv, Sw), Sr),
    find_lives(Insts, [sets:to_list(S_new_live_vars)|Lvl], [Inst|Code]).

reduce_dup(L) ->
    sets:to_list(sets:from_list(L)).

find_vars_in_arg(V) when is_atom(V) -> [V];
find_vars_in_arg(_) -> [].

get_write_var({addq, _, Arg}) -> find_vars_in_arg(Arg);
get_write_var({subq, _, Arg}) ->find_vars_in_arg(Arg);
get_write_var({negq, Arg}) -> find_vars_in_arg(Arg);
get_write_var({movq, _, Arg}) -> find_vars_in_arg(Arg);
get_write_var({popq, Arg}) -> find_vars_in_arg(Arg);
get_write_var({xorq, _, Arg}) -> find_vars_in_arg(Arg);
get_write_var({movzbq, _, Arg}) -> find_vars_in_arg(Arg);
get_write_var(_) -> [].

get_read_vars({addq, Arg1, Arg2}) ->
    find_vars_in_arg(Arg1) ++ find_vars_in_arg(Arg2);
get_read_vars({subq, Arg1, Arg2}) ->
    find_vars_in_arg(Arg1) ++ find_vars_in_arg(Arg2);
get_read_vars({negq, Arg}) -> find_vars_in_arg(Arg);
get_read_vars({movq, Arg, _}) -> find_vars_in_arg(Arg);
get_read_vars({pushq, Arg}) -> find_vars_in_arg(Arg);
get_read_vars({xorq, Arg1, Arg2}) ->
    find_vars_in_arg(Arg1) ++ find_vars_in_arg(Arg2);
get_read_vars({cmpq, Arg1, Arg2}) ->
    find_vars_in_arg(Arg1) ++ find_vars_in_arg(Arg2);
get_read_vars({movzbq, Arg, _}) -> find_vars_in_arg(Arg);
get_read_vars(_) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%% build interference %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec build_interference([x86_star_var()],
                         {[x86_star_instruction()],
                          [[x86_star_var()]]}) -> {digraph:graph(), sets:set(), [x86_star_instruction()]}.
build_interference(Vars, {Insts, Live_vars_list}) ->
    Inst_live_vars_pairs = lists:zip(Insts, Live_vars_list),
    {G, Reg_vars} = loop(Inst_live_vars_pairs, sets:new(), sets:new()),
    Graph = digraph:new([cyclic, private]),
    [digraph:add_vertex(Graph, Var) || Var <- Vars],
    [add_edge(Graph, A, B) || {A, B} <- sets:to_list(G)],
    {Graph, Reg_vars, remove_live_vars(Insts)}.

remove_live_vars([]) -> [];
remove_live_vars([{c1_if, Cmp_exp, {Insts1, _}, {Insts2, _}}|Insts]) ->
    [{c1_if, Cmp_exp, remove_live_vars(Insts1), remove_live_vars(Insts2)}
     | remove_live_vars(Insts)];
remove_live_vars([Inst|Insts]) -> [Inst|remove_live_vars(Insts)].


add_edge(G, V, D) ->
    digraph:add_edge(G, V, D),
    digraph:add_edge(G, D, V).

loop([], Graph, Reg_vars) -> {Graph, Reg_vars};
loop([{{c1_if,
        {{cmp, _Cmp}, _Arg1, _Arg2},
        {Insts1, Live_vars_then},
        {Insts2, Live_vars_else}}, _} |
       Inst_live_vars_pairs],
     Graph,
     Reg_vars) ->
    {G1, Rv1} = loop(lists:zip(Insts1, Live_vars_then), Graph, Reg_vars),
    {G2, Rv2} = loop(lists:zip(Insts2, Live_vars_else), G1, Rv1),
    loop(Inst_live_vars_pairs, G2, Rv2);
loop([{Inst, Live_vars}|Inst_live_vars_pairs], Graph, Reg_vars) ->
    case Inst of
        {movq, S, D} when is_atom(D) ->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars,
                                                      V /= D, V /= S]),
                                   Graph),
            New_reg_vars = Reg_vars;
        {addq, _S, D} when is_atom(D)->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars, V /= D]),
                                   Graph),
            New_reg_vars = Reg_vars;
        {subq, _S, D} when is_atom(D)->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars, V /= D]),
                                   Graph),
            New_reg_vars = Reg_vars;
        {callq, _L} ->
            New_graph = Graph,
            New_reg_vars = sets:union(Reg_vars, sets:from_list(Live_vars));
        {popq, D} when is_atom(D) ->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars, V /= D]),
                                   Graph),
            New_reg_vars = Reg_vars;
        {xorq, _S, D} when is_atom(D)->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars, V /= D]),
                                   Graph),
            New_reg_vars = Reg_vars;
        {movzbq, S, D} when is_atom(D) ->
            New_graph = sets:union(sets:from_list([{V, D}
                                                   || V <- Live_vars,
                                                      V /= D, V /= S]),
                                   Graph),
            New_reg_vars = Reg_vars;
        _ ->
            New_graph = Graph,
            New_reg_vars = Reg_vars
    end,
    loop(Inst_live_vars_pairs, New_graph, New_reg_vars).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec allocate_registers([x86_star_var()], digraph:graph(), sets:set())
                        -> [{x86_star_var(), register_allocate_result()}].
allocate_registers(Vars, Graph, _Reg_vars) ->
    Color_result = dict:to_list(color_graph(Vars, Graph)),
    Color_reg_map = dict:from_list([
                                    {0, {register, r12}},
                                    {1, {register, r13}},
                                    {2, {register, r14}},
                                    {3, {register, r15}},
                                    {4, {register, rbx}}
                                   ]),
    [{V, get_reg(C, Color_reg_map)} || {V, C} <- Color_result].

color_graph(Vars, Graph) ->
    Color_result = dict:new(),
    Ban_colors = dict:from_list([{Var, sets:new()}|| Var <- Vars]),
    color_loop(Ban_colors, Color_result, Graph).

color_loop(Banned_colors, Color_result, Graph) ->
    case dict:size(Banned_colors) of
        N  when N > 0 ->
            {{V, S}, New_banned_colors} = pop_max(Banned_colors),
            Color = get_color(S),
            New_color_result = dict:store(V, Color, Color_result),
            Es = [digraph:edge(Graph, E) || E <- digraph:edges(Graph, V)],
            Vs = [V2 || {_, V1, V2, _} <- Es, V1 =:= V],
            color_loop(update_banned_colors(New_banned_colors, Vs, Color),
                       New_color_result,
                       Graph);
        0 -> Color_result
    end.

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

%%%%%%%%%%%%%%%%%%%%%%%%% Assign Homes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec assign_homes([{x86_star_var(), register_allocate_result()}],
                   [x86_star_instruction()])
                  -> {integer(), [x86_star_instruction()]}.
assign_homes(Var_reg_map, Insts) ->
    Lvars = length(Var_reg_map),
    Frame_size = case Lvars rem 2 of
                     1 -> (Lvars + 1) * ?INT_SIZE;
                     0 -> Lvars * ?INT_SIZE
                 end,
     {Frame_size,
      [assign_inst_home(Inst, Var_reg_map) || Inst <- Insts]}.

assign_inst_home(Inst={set, _, _}, _Map) -> Inst;
assign_inst_home(Inst={jmp, _}, _Map) -> Inst;
assign_inst_home(Inst={jmp_if, _, _}, _Map) -> Inst;
assign_inst_home(Inst={label, _}, _Map) -> Inst;
assign_inst_home(Inst={callq, _}, _Map) -> Inst;
assign_inst_home({c1_if,
                  {{cmp, Cmp}, Arg1, Arg2},
                  Insts1,
                  Insts2}, Map) ->
    {c1_if,
     {{cmp, Cmp}, assign_arg_home(Arg1, Map), assign_arg_home(Arg2, Map)},
     [assign_inst_home(Inst, Map) || Inst <- Insts1],
     [assign_inst_home(Inst, Map) || Inst <- Insts2]};
assign_inst_home({Inst_name}, _Map) -> {Inst_name};
assign_inst_home({Inst, Arg}, Map) ->
    {Inst, assign_arg_home(Arg, Map)};
assign_inst_home({Inst, Arg1, Arg2}, Map) ->
    {Inst, assign_arg_home(Arg1, Map), assign_arg_home(Arg2, Map)}.

assign_arg_home(V, Map) when is_atom(V) ->
    case proplists:get_value(V, Map) of
        undefined -> erlang:error(can_not_find_var_in_maps);
        N when is_integer(N) -> {deref, N, rbp};
        Reg -> Reg
    end;
assign_arg_home(Arg, _Map) -> Arg.
