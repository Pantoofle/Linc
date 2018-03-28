-module(command).
-export([shutdown/0,
		 neighbours/0,
		 link/1]).

shutdown() ->
	io:fwrite("[~p] Shutting down~n", [node()]),
	init:stop().

neighbours() ->
	io:fwrite("[~p] Neighbours: ~p~n", [node(), nodes()]).

link(Node) ->
	io:fwrite("[~p] Linking to ~p~n", [node(), Node]),
	net_adm:ping(Node).
