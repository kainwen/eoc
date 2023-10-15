-module(var_server).

-behaviour(gen_server).

-export([start/0, stop/0, new_var/0, new_var/1,
         add_vars/1, get_var_list/0, delete_var/1]).

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
    gen_server:call(?SERVER, {new_var, tmp}).

new_var(Prefix) ->
    gen_server:call(?SERVER, {new_var, Prefix}).

add_vars(Vars) ->
    gen_server:call(?SERVER, {add_vars, Vars}).

get_var_list() ->
    gen_server:call(?SERVER, {get_var_list}).

delete_var(V) ->
    gen_server:call(?SERVER, {delete_var, V}).

%% Callbacks
init([]) ->
    {ok, {dict:new()}}.

handle_call({new_var, Prefix}, _From, {Dict}) ->
    {Name, New_dict} = find_new_name(Dict, Prefix),
    {reply, Name, {New_dict}};
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_new_name(Dict, Prefix) ->
    Prefix_str = atom_to_list(Prefix),
    case dict:is_key(Prefix, Dict) of
        true ->
            N = dict:fetch(Prefix, Dict),
            Name = string:join([Prefix_str, integer_to_list(N)], "."),
            New_dict = dict:update(Prefix, fun (M) -> M + 1 end, Dict),
            {list_to_atom(Name), New_dict};
        false ->
            New_dict = dict:store(Prefix, 2, Dict),
            Name = string:join([Prefix_str, integer_to_list(1)], "."),
            {list_to_atom(Name), New_dict}
    end.
