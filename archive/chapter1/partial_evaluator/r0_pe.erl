-module(r0_pe).

-include("common.hrl").

-export([pe/1]).

-spec pe(program()) -> program().
pe({program, E}) ->
    {program, pe_exp(E)}.

-spec pe_exp(exp()) -> exp().
pe_exp({minus, E}) ->
    Exp = pe_exp(E),
    case Exp of
        {int, N} ->
            {int, -N};
        _ ->
            {minus, Exp}
    end;
pe_exp({plus, E1, E2}) ->
    Exp1 = pe_exp(E1),
    Exp2 = pe_exp(E2),
    case Exp1 of
        {int, N1} ->
            case Exp2 of
                {int, N2} ->
                    {int, N1 + N2};
                _ ->
                    {plus, Exp1, Exp2}
            end;
        _ ->
            {plus, Exp1, Exp2}
    end;
pe_exp(Exp) -> Exp.
