-module(controler).
-export([sendCommand/4,
		 sendCommand/3,
		 sendCommand/2,
		 spawn/1, 
		 shutdown/1, 
		 disconnect_all/0,
		 spawn_all/1,
		 build_ring/0,
		 neighbours/0]).

% SEND A SPECIFIC COMMAND
sendCommand(Entry, Target, Command, Args) ->
	comm:send(Entry, {command, Target, Command, Args}).

sendCommand(Target, Command, Args) ->
	comm:send(lists:last(nodes()), {command, Target, Command, Args}).
	
sendCommand(Target, Command) ->
	comm:send(lists:last(nodes()), {command, Target, Command, []}).


% SETUP COMMANDS
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

disconnect_all() ->
	N = lists:last(nodes()),
	lists:foreach(fun(Node) -> erlang:disconnect_node(Node) end, nodes()),
	net_adm:ping(N).

% NODE COMMANDS
shutdown(Node) ->
	sendCommand(Node, shutdown).

% TOPOLOGY CONTROL
build_ring() ->
	lists:foldl(fun(Node, Acc) -> io:fwrite("~p -> ~p~n", [Acc, Node]), 
								  sendCommand(Acc, Acc, link, [Node]), Node end, 
				lists:last(nodes()),
				nodes()).

neighbours() ->
	sendCommand(all, neighbours).
