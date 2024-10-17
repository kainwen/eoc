-module(interp_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

interp_test() ->
    Code = "(program ()
             (let ([x (read)]) (let ([y (read)]) (+ x (- y)))))",
    Prog = parse:scan_and_parse(Code),
    Cnt = counter:new_counter(0),
    ok = meck:new(utils),
    ok = meck:expect(utils, read_from_stdin,
		     fun () ->
			     case counter:inc_counter(Cnt) of
				 0 ->
				     52;
				 1 ->
				     10
			     end
		     end),
    Val = interp:interp(Prog),
    ?assert(Val =:= {value, 42}),
    ok = counter:stop_counter(Cnt),
    ok.

-endif.
