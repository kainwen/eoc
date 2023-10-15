-module(var_server).

-include("type_spec.hrl").

-behaviour(gen_server).

-export([start/0, stop/0, new/0, new/1, add/1, get/0]).

-export([
         init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3
        ]).

-define(SERVER, ?MODULE).

%% APIs
-spec start() -> ok.
start() ->
    case whereis(?SERVER) of
        undefined ->
            {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
            ok;
        _ ->
            ok = stop(),
            start()
    end.

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, {stop}).

-spec add([{atom(), c2_type()}]) -> ok.
add(Var_type_list) ->
    gen_server:call(?SERVER, {add, Var_type_list}).

-spec get() -> [{atom(), c2_type()}].
get() ->
    gen_server:call(?SERVER, {get}).

-spec new() -> atom().
new() ->
    new(tmp).

-spec new(atom()) -> atom().
new(Prefix) ->
    gen_server:call(?SERVER, {new, Prefix}).

%% Callbacks
-type state() :: {sets:set(), [{atom(), c2_type()}]}.

-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, {sets:new(), []}}.

handle_call({new, Prefix}, _From, {Nset, VTs}) ->
    N = get_number(Nset),
    Name = atom_to_list(Prefix) ++ integer_to_list(N),
    {reply, list_to_atom(Name), {sets:add_element(N, Nset), VTs}};
handle_call({add, Var_type_list}, _From, {Nset, VTs}) ->
    {reply, ok, {Nset, Var_type_list ++ VTs}};
handle_call({get}, _From, {Nset, VTs}) ->
    {reply, VTs, {Nset, VTs}};
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
-spec get_number(sets:set()) -> integer().
get_number(Set) ->
    N = 30000 + random:uniform(9999),
    case sets:is_element(N, Set) of
        true ->
            get_number(Set);
        false ->
            N
    end.
