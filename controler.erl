-module(controler).
-export([sendCommand/4,
		 spawn/1, shutdown/1, spawn_all/1]).

sendCommand(Entry, Target, Command, Args) ->
	comm:send(Entry, {command, Target, Command, Args}).

spawn(Node) ->
	net_adm:ping(Node),
	c:c(setup),
	setup:bootstrap(Node).

spawn_all(0) ->
	io:fwrite("Spawning done~n");

spawn_all(Id) ->
	Host = lists:nthtail(1, string:find(atom_to_list(node()), "@")),
	?MODULE:spawn(list_to_atom(lists:concat(["n", Id, "@", Host]))),
	?MODULE:spawn_all(Id-1).

shutdown(Node) ->
	comm:send(lists:last(nodes()), {command, Node, shutdown, []}).



