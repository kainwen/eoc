-module(expose_allocation).

-include("type_spec.hrl").

-export([expose_allocation/1]).

-define(DATA_SIZE, 8).
-define(TAG_SIZE, 8).

-spec expose_allocation(r3_program()) -> r3_ext_program().
expose_allocation({r3_program, Exp}) ->
    var_server:start(),
    New_exp = expose_allocation_exp(Exp),
    var_server:stop(),
    {r3_ext_program, New_exp}.

-spec expose_allocation_exp(r3_exp()) -> r3_ext_exp().
expose_allocation_exp({int, N}) -> {int, N};
expose_allocation_exp({var, V}) -> {var, V};
expose_allocation_exp({'let', Vars, Exps, Body}) ->
    {'let',
     Vars,
     [expose_allocation_exp(Exp) || Exp <- Exps],
     expose_allocation_exp(Body)};
expose_allocation_exp({tuple_exp, Exps}) ->
    Len = length(Exps),
    Bytes = ?TAG_SIZE + Len * ?DATA_SIZE,
    Exposed_exps = lists:map(fun expose_allocation_exp/1, Exps),
    Vec_init_vars = [var_server:new(vecinit) || _ <- Exps],
    Var_for_testgc = var_server:new('dummy'),
    Exp_for_testgc = {if_exp,
                      {{cmp, '<'},
                       {plus, {global_value, free_ptr}, {int, Bytes}},
                       {global_value, fromspace_end}},
                      {void},
                      {collect, Bytes}},
    Vector_var = var_server:new(vector),
    Allocate = {allocate, Len, place_holder},
    Vec_init_ret_vars = [var_server:new(initret) || _ <- Exps],
    Vec_init_exps = [{vector_set_exp, {var, Vector_var}, I-1, {var, V}}
                     || {I, V} <- lists:zip(lists:seq(1, Len), Vec_init_vars)],
    {'let',
     Vec_init_vars,
     Exposed_exps,
     {'let',
      [Var_for_testgc],
      [Exp_for_testgc],
      {'let',
       [Vector_var],
       [Allocate],
       {'let',
        Vec_init_ret_vars,
        Vec_init_exps,
        {var, Vector_var}}}}};
expose_allocation_exp(Exp) -> expose_helper(Exp).

-spec expose_helper(r3_exp()) -> r3_ext_exp().
expose_helper({A}) -> {A};
expose_helper(Exp) when tuple_size(Exp) > 1 ->
    Op = element(1, Exp),
    Ts = tuple_size(Exp),
    Sub_exps = [case element(I, Exp) of
                    N when is_integer(N) -> N;
                    Sub_exp -> expose_allocation_exp(Sub_exp)
                end
                || I <- lists:seq(2, Ts)],
    list_to_tuple([Op|Sub_exps]).
