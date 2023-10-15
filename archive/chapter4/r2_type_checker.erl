-module(r2_type_checker).

-include("common.hrl").

-export([typer/1]).

-spec typer(r2_exp()) -> r2_type().
typer(Exp) ->
    typer(Exp, []).

typer({int, _N}, _) -> int;
typer({read}, _) -> int;
typer(Exp={minus, Exp1}, Tenv) ->
    Tp = typer(Exp1, Tenv),
    assert_type(Tp, int, Exp),
    int;
typer(Exp={plus, Exp1, Exp2}, Tenv) ->
    Tp1 = typer(Exp1, Tenv),
    assert_type(Tp1, int, Exp),
    Tp2 = typer(Exp2, Tenv),
    assert_type(Tp2, int, Exp),
    int;
typer({var, V}, Tenv) ->
    case proplists:get_value(V, Tenv) of
        undefined ->
            erlang:error({can_not_find_var_type, V});
        Tp -> Tp
    end;
typer({'let', Vars, Exps, Body}, Tenv) ->
    Tps = [typer(Exp, Tenv) || Exp <- Exps],
    New_tenv = lists:zip(Vars, Tps) ++ Tenv,
    typer(Body, New_tenv);
typer({true_exp}, _) -> bool;
typer({false_exp}, _) -> bool;
typer(Exp={and_exp, Exp1, Exp2}, Tenv) ->
    Tp1 = typer(Exp1, Tenv),
    assert_type(Tp1, bool, Exp),
    Tp2 = typer(Exp2, Tenv),
    assert_type(Tp2, bool, Exp),
    bool;
typer(Exp={not_exp, Exp1}, Tenv) ->
    Tp = typer(Exp1, Tenv),
    assert_type(Tp, bool, Exp),
    bool;
typer(Exp={{cmp, 'eq?'}, Exp1, Exp2}, Tenv) ->
    Tp1 = typer(Exp1, Tenv),
    Tp2 = typer(Exp2, Tenv),
    assert_type(Tp1, Tp2, Exp),
    bool;
typer(Exp={{cmp, _}, Exp1, Exp2}, Tenv) ->
    Tp1 = typer(Exp1, Tenv),
    assert_type(Tp1, int, Exp),
    Tp2 = typer(Exp2, Tenv),
    assert_type(Tp2, int, Exp),
    bool;
typer(Exp={if_exp, A, B, C}, Tenv) ->
    Tp_a = typer(A, Tenv),
    assert_type(Tp_a, bool, Exp),
    Tp_b = typer(B, Tenv),
    Tp_c = typer(C, Tenv),
    assert_type(Tp_b, Tp_c, Exp),
    Tp_b.

assert_type(T1, T2, Exp) ->
    case T1 =:= T2 of
        true ->
            ok;
        false ->
            erlang:error({type_error, T1, T2, Exp})
    end.
