-module(read_index).

-behaviour(gen_server).

-export([start/0, stop/0, get_index/0]).

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

get_index() ->
    gen_server:call(?SERVER, {get_index}).

%% Callbacks
init([]) ->
    {ok, 0}.

handle_call({get_index}, _From, N) ->
    {reply, N, N+1};
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
