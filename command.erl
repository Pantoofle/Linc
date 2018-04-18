-module(command).
-export([shutdown/0,
		 neighbours/0,
		 link/1]).

shutdown() ->
	io:fwrite("[~p] Shutting down~n", [node()]),
	lists:foreach(fun(Node) -> comm:send(Node, {dead, node()}) end, nodes()),
	init:stop().

neighbours() ->
	io:fwrite("[~p] Neighbours : ~p~n", [node(), nodes()]).

link(Node) ->
	comm:send_to_back({link, Node}).
