-module(counter_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

counter_test() ->
    Cnt = counter:new_counter(0),
    N1 = counter:inc_counter(Cnt),
    ?assert(N1 =:= 0),
    N2 = counter:inc_counter(Cnt),
    ?assert(N2 =:= 1),
    ok = counter:stop_counter(Cnt).

-endif.
