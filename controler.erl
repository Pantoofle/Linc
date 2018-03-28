-module(controler).
-export([sendCommand/3, sendMsg/3,
		 spawn/1, shutdown/1, spawn_all/1]).

sendCommand(Entry, Target, {Module, Fun, Args}) ->
	{listener, Entry} ! {command, Target, {Module, Fun, Args}, rand:uniform(4096)}.

sendMsg(Entry, Target, Msg) ->
	{listener, Entry} ! {command, Target, {io, fwrite, [Msg, []]}, rand:uniform(4096)}.

spawn(Node) ->
	net_adm:ping(Node),
	c:c(setup),
	setup:bootstrap(Node).

spawn_all(0) ->
	io:fwrite("Spawning done~n");

spawn_all(Id) ->
	?MODULE:spawn(list_to_atom(lists:concat(["n", Id, "@ouranos"]))),
	?MODULE:spawn_all(Id-1).

shutdown(all) ->
	{listener, lists:last(nodes())} ! {control, all, shutdown};
shutdown(Node) ->
	{listener, Node} ! {control, Node, shutdown}.



