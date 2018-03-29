-module(comm).
-export([hash/1, broadcast/1, send/2]).

hash(Msg) ->
	S = io_lib:format("~p", [Msg]),
	crypto:hash(md5, lists:flatten(S)).

broadcast(Msg) ->
	lists:foreach(fun(Target) -> send(Target, Msg) end, nodes()).


send(all, Msg) ->
	Rnd = rand:uniform(4096),
	lists:foreach(fun(Node) -> {listener, Node} ! {Msg, Rnd} end, nodes());
send(Node, Msg) ->
	{listener, Node} ! {Msg, rand:uniform(4096)}.
