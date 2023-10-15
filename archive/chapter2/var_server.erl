-module(var_server).

-behaviour(gen_server).

-export([start/0, stop/0, new_var/0, add_vars/1, get_var_list/0, delete_var/1]).

-export([
         init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3
        ]).

-define(SERVER, ?MODULE).

%% APIs
start() ->
    case whereis(?SERVER) of
        undefined ->
            gen_server:start_link({local, ?SERVER}, ?MODULE, [], []);
        _ ->
            ok = stop(),
            start()
    end.

stop() ->
    gen_server:call(?SERVER, {stop}).

new_var() ->
    gen_server:call(?SERVER, {new_var}).

add_vars(Vars) ->
    gen_server:call(?SERVER, {add_vars, Vars}).

get_var_list() ->
    gen_server:call(?SERVER, {get_var_list}).

delete_var(V) ->
    gen_server:call(?SERVER, {delete_var, V}).

%% Callbacks
init([]) ->
    {ok, {1, []}}.

handle_call({new_var}, _From, {N, Var_list}) ->
    {New_name, M} = find_new_name(N, Var_list),
    {reply, New_name, {M, [New_name|Var_list]}};
handle_call({add_vars, Vars}, _From, {N, Var_list}) ->
    {reply, ok, {N, Vars ++ Var_list}};
handle_call({get_var_list}, _From, State={_N, Var_list}) ->
    {reply, Var_list, State};
handle_call({delete_var, V}, _From, {N, Var_list}) ->
    {reply, ok, {N, lists:delete(V, Var_list)}};
handle_call({stop}, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
{ok, State}.

%%%%%%%%%%%%%%%%%% Internal Functions %%%%%%%%%%%%%%%%%%%%%
find_new_name(N, Var_list) ->
    Name = list_to_atom(string:join(["tmp", integer_to_list(N)], ".")),
    case lists:member(Name, Var_list) of
        true -> find_new_name(N+1, Var_list);
        false -> {Name, N+1}
    end.
