-module(r0_pe_advanced).

-include("common.hrl").

-export([pe_advanced/1]).

-type sign() :: -1 | +1.

-type reduced_atom() :: {int, integer()}
                      |{read}
                      | {minus, {read}}.

-spec pe_advanced(program()) -> program().
pe_advanced({program, Exp}) ->
    {program, pe_advanced_exp(Exp)}.

-spec pe_advanced_exp(exp()) -> exp().
pe_advanced_exp(Exp) ->
    E_list = pe_advanced_exp(Exp, +1),
    All_ints = [element(2, E) || E <- E_list, element(1, E) =:= int],
    All_not_ints = [E || E <- E_list, element(1, E) /= int],
    Final_int = {int, lists:sum(All_ints)},
    Final_reads = combine(All_not_ints),
    case Final_reads of
        {int, 0} ->
            Final_int;
        _ ->
            case Final_int of
                {int, 0} ->
                    Final_reads;
                _ ->
                    {plus, Final_int, Final_reads}
            end
    end.

-spec pe_advanced_exp(exp(), sign()) -> [reduced_atom()].
pe_advanced_exp({int, N}, Current_sign) ->
    [{int, Current_sign * N}];
pe_advanced_exp({read}, +1) ->
    [{read}];
pe_advanced_exp({read}, -1) ->
    [{minus, {read}}];
pe_advanced_exp({minus, {read}}, +1) ->
    [{minus, {read}}];
pe_advanced_exp({minus, {read}}, -1) ->
    [{read}];
pe_advanced_exp({minus, E}, Current_sign) ->
    pe_advanced_exp(E, -Current_sign);
pe_advanced_exp({plus, E1, E2}, Current_sign) ->
    E1_list = pe_advanced_exp(E1, Current_sign),
    E2_list = pe_advanced_exp(E2, Current_sign),
    E1_list ++ E2_list.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec combine([reduced_atom()]) -> exp().
combine([]) -> {int, 0};
combine([R|Es]) ->
    Result = combine(Es),
    case Result of
        {int, 0} -> R;
        _ -> {plus, R, Result}
    end.
