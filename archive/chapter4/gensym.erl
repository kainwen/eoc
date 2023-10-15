-module(gensym).

-behaviour(gen_server).

-export([start/0, stop/0, new_sym/0, new_sym/1]).

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

new_sym() ->
    new_sym(tmp).

new_sym(Prefix) ->
    gen_server:call(?SERVER, {new_sym, Prefix}).

%% Callbacks
init([]) ->
    Hist = sets:new(),
    {ok, {Hist}}.

handle_call({new_sym, Prefix}, _From, {Hist}) ->
    {N, New_hist} = get_rand(Hist),
    Sym = string:join([atom_to_list(Prefix),
                       integer_to_list(N)], ""),
    {reply, list_to_atom(Sym), {New_hist}};
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
get_rand(S) ->
    N = random:uniform(99999),
    case N < 10000 of
        true ->
            get_rand(S);
        false ->
            New_S = sets:add_element(N, S),
            {N, New_S}
    end.
