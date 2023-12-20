-module(utils).

-export([read_from_stdin/0]).

-spec read_from_stdin() -> integer().
read_from_stdin() ->
    {ok, [N]} = io:fread("", "~d"),
    N.
