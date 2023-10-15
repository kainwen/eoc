-module(select_instructions).

-include("type_spec.hrl").

-export([select_instructions/1]).

-define(DATA_SIZE, 8).


-type bit_int() :: 1 | 0.


-spec select_instructions(c2_program()) -> x86_star_program().
select_instructions({c2_program, Var_type_list, {type, Type}, Sts}) ->
    {x86_star_program,
     Var_type_list,
     {type, Type},
     lists:flatten([transform(replace_bool_in_st(St))
                    || St <- Sts])}.

-spec transform(c2_statement()) -> [x86_star_instruction()].
transform(St={assign, _, _}) ->
    transform_assign(St);
transform(St={return, _}) ->
    transform_return(St);
transform(St={c2_if, _, _, _}) ->
    transform_if(St);
transform(St={collect, _}) ->
    transform_collect(St).


transform_assign({assign, V, {int, N}}) ->
    [{movq, {int, N}, V}];
transform_assign({assign, V, V1}) when is_atom(V1) ->
    [{movq, V1, V}];
transform_assign({assign, V, {read}}) ->
    [{callq, read_int},
     {movq, {register, rax}, V}];
transform_assign({assign, V, {minus, C1_arg}}) when V /= C1_arg ->
    [{negq, C1_arg},
     {movq, C1_arg, V}];
transform_assign({assign, V, {minus, C1_arg}}) when V =:= C1_arg ->
    [{negq, C1_arg}];
transform_assign({assign, V, {plus, Arg1, Arg2}}) when V =:= Arg1 ->
    [{addq, Arg2, V}];
transform_assign({assign, V, {plus, Arg1, Arg2}}) when V =:= Arg2 ->
    [{addq, Arg1, V}];
transform_assign({assign, V, {plus, Arg1, Arg2}}) ->
    [{movq, Arg1, V},
     {addq, Arg2, V}];
transform_assign({assign, V, {not_op, C1_arg}}) ->
    [{movq, C1_arg, V},
     {xorq, {int, 1}, C1_arg}];
transform_assign({assign, V, {{cmp, Cmp}, Arg1, Arg2}}) ->
    [{cmpq, Arg2, Arg1},
     {set,
      get_cond_code_from_cmp(Cmp),
      {byte_reg, {register, al}}},
     {movzbq, {byte_reg, {register, al}}, V}];
transform_assign({assign, V, {allocate, N, Type}}) ->
    [{movq, {global_value, free_ptr}, V},
     {addq, {int, ?DATA_SIZE * (N + 1)}, {global_value, free_ptr}},
     {movq, V, {register, r11}},
     {movq, {int, gen_tag(Type)}, {deref, {register, r11}, 0}}];
transform_assign({assign, V, {vector_ref, Arg, N}}) ->
    [{movq, Arg, {register, 11}},
     {movq, {deref, {register, r11}, ?DATA_SIZE * (N+1)}, V}];
transform_assign({assign, V, {vector_set, Arg1, N, Arg2}}) ->
    [{movq, Arg1, {register, r11}},
     {movq, Arg2, {deref, {register, r11}, ?DATA_SIZE * (N+1)}},
     {movq, {int, 0}, V}];
transform_assign({assign, V, {void}}) ->
    [{movq, {int, 0}, V}];
transform_assign({assign, V, {global_value, Name}}) ->
    [{movq, {global_value, Name}, V}].

transform_return({return, Arg}) ->
    [{movq, Arg, {register, rax}}].

transform_if({c2_if, Cmp_exp, Sts1, Sts2}) ->
    {c2_if,
     Cmp_exp,
     lists:flatten([transform(St) || St <- Sts1]),
     lists:flatten([transform(St) || St <- Sts2])}.

transform_collect({collect, Bytes}) ->
    [{movq, {register, r15}, {register, rdi}},
     {movq, Bytes, {register, rsi}},
     {callq, collect}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_cond_code_from_cmp(Cmp) ->
    Dict = [
            {'eq?', e},
            {'>', g},
            {'<', l},
            {'>=', ge},
            {'=<', le}
           ],
    proplists:get_value(Cmp, Dict).

-spec gen_tag({tuple, [c2_type()]}) -> integer().
gen_tag({tuple, Tps}) ->
    Len = length(Tps),
    Pointer_mask_list = [case(Tp) of
                             {tuple, _} -> 1;
                             _ -> 0
                         end
                         || Tp <- Tps],
    Pointer_mask = get_integer_from_binlist(Pointer_mask_list),
    (Len bsl 1) + (Pointer_mask bsl 7) + 1.

-spec get_integer_from_binlist([bit_int()]) -> integer().
get_integer_from_binlist(Bits) ->
    get_integer_from_binlist(Bits, 0).

get_integer_from_binlist([], N) -> N;
get_integer_from_binlist([Bit|Bits], N) ->
    get_integer_from_binlist(Bits, N * 2 + Bit).

replace_bool_in_st(St) ->
    St_as_list = tuple_to_list(St),
    list_to_tuple([case Element of
                       {bool, c2_true} ->
                           {int, 1};
                       {bool, c2_false} ->
                           {int, 0};
                       L when is_list(L) ->
                           [replace_bool_in_st(E) || E <- L];
                       T -> T
                   end
                   || Element <- St_as_list]).
