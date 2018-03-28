-module(comm).
-export([hash/1, broadcast/1]).

hash(Msg) ->
	S = io_lib:format("~p", [Msg]),
	crypto:hash(md5, lists:flatten(S)).

broadcast(Msg) ->
	lists:foreach(fun(Target) -> {listener, Target} ! Msg end, nodes()).
