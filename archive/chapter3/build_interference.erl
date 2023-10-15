-module(build_interference).

-include("common.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([build_interference/1]).

-spec build_interference(x86_64_theta_program()) -> x86_64_gamma_program().
build_interference({x86_64_theta_program, {Vars, Live_vars_list}, Insts}) ->
    Inst_live_vars_pairs = lists:zip(Insts, Live_vars_list),
    {G, Reg_vars} = loop(Inst_live_vars_pairs, sets:new(), sets:new()),
    Graph = digraph:new([cyclic, private]),
    [digraph:add_vertex(Graph, Var) || Var <- Vars],
    [add_edge(Graph, A, B) || {A, B} <- sets:to_list(G)],
    {x86_64_gamma_program, {Vars, Graph, Reg_vars}, Insts}.

add_edge(G, V, D) ->
    digraph:add_edge(G, V, D),
    digraph:add_edge(G, D, V).

loop([], Graph, Reg_vars) -> {Graph, Reg_vars};
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
        _ ->
            New_graph = Graph,
            New_reg_vars = Reg_vars
    end,
    loop(Inst_live_vars_pairs, New_graph, New_reg_vars).


%% build_interference_test() ->
%%     X86_64_star_program = {x86_64_star_program, [v, w, x, y, z, 't.1', 't.2'],
%%                            [{movq, {int, 1}, v},
%%                             {movq, {int, 46}, w},
%%                             {movq, v, x},
%%                             {addq, {int, 7}, x},
%%                             {movq, x, y},
%%                             {addq, {int, 4}, y},
%%                             {movq, x, z},
%%                             {addq, w, z},
%%                             {movq, y, 't.1'},
%%                             {negq, 't.1'},
%%                             {movq, z, 't.2'},
%%                             {addq, 't.1', 't.2'},
%%                             {movq, 't.2', {register, rax}}]},
%%     X86_64_theta_program = uncover_live:uncover_live(X86_64_star_program),
%%     {_, {_, G, _R}, _} = build_interference(X86_64_theta_program),
%%     S = get_edges(G),
%%     Std = sets:from_list([{v, w},
%%                           {w, v},
%%                           {w, y},
%%                           {y, w},
%%                           {w, x},
%%                           {x, w},
%%                           {w, z},
%%                           {z, w},
%%                           {y, z},
%%                           {z, y},
%%                           {x, y},
%%                           {y, x},
%%                           {z, 't.1'},
%%                           {'t.1', z},
%%                           {'t.1', 't.2'},
%%                           {'t.2', 't.1'}]),
%%     ?assert(sets:is_subset(S, Std) and sets:is_subset(Std, S)).

%% get_edges(G) ->
%%     Es = digraph:edges(G),
%%     Rs = [digraph:edge(G, E) || E <- Es],
%%     sets:from_list([{A, B} || {_, A, B, _} <- Rs]).
