-module(comm).
-export([hash/1, 
		 broadcast/1,
		 send/2, 
		 forward_all/1,
		 send_to_back/1,
		 send_to_front/1]).

hash(Msg) ->
	S = io_lib:format("~p", [Msg]),
	crypto:hash(md5, lists:flatten(S)).

broadcast(Msg) ->
	send(all, Msg).

forward_all(Msg) ->
	lists:foreach(fun(Target) -> {back, Target} ! Msg end, nodes()).

send(all, Msg) ->
	Rnd = rand:uniform(4096),
	lists:foreach(fun(Node) -> {back, Node} ! {Msg, Rnd} end, nodes());

send(Node, Msg) ->
	case ets:lookup(linc_route, Node) of
		[] -> send(all, {whereis, Node}), ets:insert(linc_wait, {Node, Msg}), ok;
		[{_, _, By} | _] -> {back, By} ! {Msg, rand:uniform(4096)}, wait
	end.

send_to_front(Msg) -> 
	{front, node()} ! Msg.

send_to_back(Msg) -> 
	send(node(), Msg).
