-module(setup).
-export([bootstrap/1,
		 setup_front/0,
		 setup_back/0,
		 spawn_all/1,
		 build_ring/0,
		 disconnect_all/0]).

bootstrap(Node) ->
	net_adm:ping(Node),
	Modules = [front,
			   back,
			   comm,
			   command,
			   storage,
			   setup],
 
	lists:foreach(fun(Mod) -> load_remote_code(Node, Mod) end, Modules),
	spawn(Node, ?MODULE, setup_back, []),
	spawn(Node, ?MODULE, setup_front, []).

% SETUP COMMANDS
spawn_all(0) ->
	io:fwrite("Spawning done~n");
spawn_all(Id) ->
	Host = lists:nthtail(1, string:find(atom_to_list(node()), "@")),
	bootstrap(list_to_atom(lists:concat(["n", Id, "@", Host]))),
	?MODULE:spawn_all(Id-1).

disconnect_all() ->
	N = lists:last(nodes()),
	io:fwrite("Disconnecting from all nodes~n"),
	comm:send(all, {dead, node()}),
	lists:foreach(fun(Node) -> erlang:disconnect_node(Node) end, nodes()),
	io:fwrite("Reconnecting to node ~p~n", [N]),
	timer:apply_after(50, net_adm, ping, [N]),
	ok.

load_remote_code(Node, Module) ->
	c:c(Module),
	{Mod, Bin, File} = code:get_object_code(Module),
	spawn(Node, code, load_binary, [Mod, File, Bin]).

setup_front() ->
	io:fwrite("[~p] Setup front.~n", [node()]),
	case lists:member(front, registered()) of
		true -> unregister(front), register(front, self());
		false-> register(front, self())
	end,
	front:start().

setup_back() ->
	io:fwrite("[~p] Setup back.~n", [node()]),
	case lists:member(back, registered()) of
		true -> unregister(back), register(back, self());
		false-> register(back, self())
	end,
	back:start().

% TOPOLOGY CONTROL
build_ring() ->
	case lists:member(linc_route, ets:all()) of
		true  -> ok;
		false -> ets:new(linc_route, [set, public, named_table])
	end,
	lists:foreach(fun(Node) -> ets:insert(linc_route, {Node, 1, Node}) end, nodes()),
	lists:foldl(fun(Node, Acc) -> io:fwrite("~p -> ~p~n", [Acc, Node]), 
								  comm:send(Acc, {link, Node}), Node end, 
				lists:last(nodes()),
				nodes()),
	ets:delete(linc_route).

