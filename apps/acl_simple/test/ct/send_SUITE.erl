-module(send_SUITE).
-author("Mykhailo Krasniuk <miha.190901@gmail.com>").
-include_lit("common_test/include/ct.hrl").

-export([all/0]).
-export([test1/1, test2/1]).

all() ->
    [].

test1(_Config) ->
    1 = 1.

test2(_Config) ->
    1/0.