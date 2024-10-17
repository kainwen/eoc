-module(counter).

-export([new_counter/1, inc_counter/1, stop_counter/1]).
-export([counter/1]).

-export_type([counter/0]).

-opaque counter() :: pid().

-spec new_counter(integer()) -> counter().
new_counter(N) ->
    Proc = spawn(?MODULE, counter, [N]),
    Proc.

-spec inc_counter(counter()) -> integer().
inc_counter(Cnt) ->
    Ref = make_ref(),
    Cnt ! {add, self(), Ref},
    receive
	{Ref, N} ->
	    N
    end.

-spec stop_counter(counter()) -> ok.
stop_counter(Cnt) ->
    Cnt ! {stop},
    ok.

%%% Internal %%%
-spec counter(integer()) -> ok.
counter(N) ->
    receive
	{add, Pid, Ref} ->
	    Pid ! {Ref, N},
	    counter(N+1);
	{stop} ->
	    ok
    end.
