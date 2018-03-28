-module(setup).
-export([bootstrap/1,
		 setup/1]).

bootstrap(Node) ->
	Modules = [agent,
			   comm,
			   command,
			   setup],
 
	lists:foreach(fun(Mod) -> load_remote_code(Node, Mod) end, Modules),
	spawn(Node, ?MODULE, setup, [node()]).

load_remote_code(Node, Module) ->
	c:c(Module),
	{Mod, Bin, File} = code:get_object_code(Module),
	spawn(Node, code, load_binary, [Mod, File, Bin]).

setup(Node) ->
	io:fwrite("Setup the node ~p. Linked to ~p~n", [node(), Node]),
	net_adm:ping(Node),
	case lists:member(listener, registered()) of
		true -> unregister(listener), register(listener, self());
		false-> register(listener, self())
	end,
	agent:listen().
