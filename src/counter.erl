-module(counter).
-behaviour(gen_server).

-include("types.hrl").

%% Exported APIs
-export([new/1, bump/1, fetch/1, shutdown/1]).
%% Exported callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-spec new(integer()) -> counter().
new(StartNumber) ->
    {ok, Pid} = gen_server:start_link(?MODULE, StartNumber, []),
    Pid.

-spec bump(counter()) -> ok.
bump(Counter) ->
    ok = gen_server:call(Counter, bump).

-spec fetch(counter()) -> integer().
fetch(Counter) ->
    gen_server:call(Counter, fetch).

-spec shutdown(counter()) -> ok.
shutdown(Counter) ->
    ok = gen_server:stop(Counter).

%% Callbacks
init(StartNumber) ->
    {ok, StartNumber}.

handle_call(bump, _From, N) ->
    {reply, ok , N+1};
handle_call(fetch, _From, N) ->
    {reply, N, N}.

handle_cast(_, S) -> {noreply, S}.

terminate(_Reason, _State) ->
    ok.
