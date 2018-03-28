-module(command).
-export([shutdown/0,
		 neighbours/0]).

shutdown() ->
	io:fwrite("[~p] Shutting down~n", [node()]),
	init:stop().

neighbours() ->
	io:fwrite("[~p] Neighbours: ~p~n", [node(), nodes()]).
