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
	lists:foreach(fun(Target) -> send(Target, Msg) end, nodes()).

forward_all(Msg) ->
	lists:foreach(fun(Target) -> {back, Target} ! Msg end, nodes()).

send(all, Msg) ->
	Rnd = rand:uniform(4096),
	lists:foreach(fun(Node) -> {back, Node} ! {Msg, Rnd} end, nodes());

send(Node, Msg) ->
	case lists:member(Node, [node() | nodes()] ) of
		true ->	{back, Node} ! {Msg, rand:uniform(4096)};
		false -> unknown_node
	end.

send_to_front(Msg) -> 
	{front, node()} ! Msg.

send_to_back(Msg) -> 
	send(node(), Msg).
