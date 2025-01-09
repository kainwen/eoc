-module(explicate_control).

-export([explicate_control/1]).

-include("ast.hrl").

-spec explicate_control(mon_program()) -> cvar_program().
explicate_control(#mon_program{body=MonExp}) ->
    StartLabel = start,
    #cvar_program{body=[{StartLabel, ec(MonExp)}]}.

-spec ec(mon_exp()) -> cvar_tail().
ec(MonExp) ->
    {CvarExp, Assignments} = ec(MonExp, []),
    Return = #cvar_return{exp=CvarExp},
    build_final_tail(Return, Assignments).

-spec ec(mon_exp(), [cvar_stmt()]) -> {cvar_exp(), [cvar_stmt()]}.
ec(MonExp, Assignments) ->
    case MonExp of
	#mon_let_exp{} ->
	    ec_mon_let(MonExp, Assignments);
	_ ->
	    {MonExp, Assignments}
    end.

-spec ec_mon_let(mon_exp(), [cvar_stmt()]) -> {cvar_exp(), [cvar_stmt()]}.
ec_mon_let(#mon_let_exp{bindings=Bindings, body=Body}, Assignments) ->
    NewAssignments = ec_mon_let_bindings(Bindings, []),
    ec(Body, Assignments ++ NewAssignments).

-spec ec_mon_let_bindings([mon_binding()], [cvar_stmt()]) -> [cvar_stmt()].
ec_mon_let_bindings([], Assignments) -> Assignments;
ec_mon_let_bindings([{Var, MonExp}|Bindings], Assignments) -> 
    {CvarExp, NewAssignments} = ec(MonExp, Assignments),
    ec_mon_let_bindings(Bindings,
			NewAssignments ++ [#cvar_assign{var=Var, exp=CvarExp}]).

-spec build_final_tail(#cvar_return{}, [cvar_stmt()]) -> cvar_tail().
build_final_tail(Return, []) -> Return;
build_final_tail(Return, [Assign|Assignments]) ->
    Tail = build_final_tail(Return, Assignments),
    #cvar_seq{stmt=Assign, tail=Tail}.
