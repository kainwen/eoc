-module(uncover_live).

-include("common.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([uncover_live/1]).

-spec uncover_live(x86_64_star_program()) -> x86_64_theta_program().
uncover_live({x86_64_star_program, X86_64_star_vars, Insts}) ->
    R_insts = lists:reverse(Insts),
    Lives = find_lives(R_insts, [[]]),
    {x86_64_theta_program, {X86_64_star_vars, Lives}, Insts}.

find_lives([_], Live_vars_list) -> Live_vars_list;
find_lives([Inst|Insts], Lvl=[Live_vars|_Live_vars_list]) ->
    Write_vars = get_write_var(Inst),
    Read_vars = get_read_vars(Inst),
    Sw = sets:from_list(Write_vars),
    Sr = sets:from_list(Read_vars),
    Slv = sets:from_list(Live_vars),
    S_new_live_vars = sets:union(sets:subtract(Slv, Sw), Sr),
    find_lives(Insts, [sets:to_list(S_new_live_vars)|Lvl]).

get_write_var({addq, _, Arg}) -> get_var_from_arg(Arg);
get_write_var({subq, _, Arg}) ->get_var_from_arg(Arg);
get_write_var({negq, Arg}) -> get_var_from_arg(Arg);
get_write_var({movq, _, Arg}) -> get_var_from_arg(Arg);
get_write_var({popq, Arg}) -> get_var_from_arg(Arg);
get_write_var(_) -> [].

get_read_vars({addq, Arg1, Arg2}) ->
    get_var_from_arg(Arg1) ++ get_var_from_arg(Arg2);
get_read_vars({subq, Arg1, Arg2}) ->
    get_var_from_arg(Arg1) ++ get_var_from_arg(Arg2);
get_read_vars({negq, Arg}) -> get_var_from_arg(Arg);
get_read_vars({movq, Arg, _}) -> get_var_from_arg(Arg);
get_read_vars({pushq, Arg}) -> get_var_from_arg(Arg);
get_read_vars(_) -> [].

get_var_from_arg({int, _N}) -> [];
get_var_from_arg({register, _R}) -> [];
get_var_from_arg(V) when is_atom(V) -> [V].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% uncover_live_test() ->
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
%%     Std_answer = [[v],
%%                   [v, w],
%%                   [w, x],
%%                   [w, x],
%%                   [w, x, y],
%%                   [w, x, y],
%%                   [w, y, z],
%%                   [y, z],
%%                   ['t.1', z],
%%                   ['t.1', z],
%%                   ['t.1', 't.2'],
%%                   ['t.2'],
%%                   []],
%%     X86_64_theta_program = uncover_live(X86_64_star_program),
%%     {x86_64_theta_program, {_, Live_vars_list}, _} = X86_64_theta_program,
%%     [?assert(sets:is_subset(sets:from_list(S1), sets:from_list(S2)) and
%%              sets:is_subset(sets:from_list(S2), sets:from_list(S1)))
%%      || {S1, S2} <- lists:zip(Live_vars_list, Std_answer)].
