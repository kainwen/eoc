-module(name_server).

-export([get_name/1, start/0, loop/1]).

start() ->
    case whereis(name_server) of
        undefined ->
            Pid = spawn(?MODULE, loop, [[]]),
            true = register(name_server, Pid);
        _ ->
            name_server ! {stop, self()},
            receive
                ok -> start()
            end
    end.

get_name(V) ->
    name_server ! {new_name, V, self()},
    receive
        N -> N
    end.

loop(Var_name_plist) ->
    receive
        {new_name, V, From} ->
            case proplists:get_value(V, Var_name_plist) of
                undefined ->
                    From ! 1,
                    loop([{V, 1}|Var_name_plist]);
                N ->
                    New_name = N + 1,
                    From ! New_name,
                    New_list = proplists:delete(V, Var_name_plist),
                    loop([{V, New_name}|New_list])
            end;
        {stop, From} ->
            true = unregister(name_server),
            From ! ok
    end.
