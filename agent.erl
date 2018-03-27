-module(agent).
-export([listen/0, bootstrap/1, handle/1, setup/1, hash_msg/1]).

bootstrap(Node) ->
	{Mod, Bin, File} = code:get_object_code(?MODULE),
	spawn(Node, code, load_binary, [Mod, File, Bin]),
	spawn(Node, ?MODULE, setup, [node()]).

setup(Node) ->
	io:fwrite("Setup the node ~p. Linked to ~p~n", [node(), Node]),
	net_adm:ping(Node),
	case lists:member(listener, registered()) of
		true -> unregister(listener), register(listener, self());
		false-> register(listener, self())
	end,
	listen().

hash_msg(Msg) ->
	S = io_lib:format("~p", [Msg]),
	crypto:hash(md5, lists:flatten(S)).

listen() -> listen([]).
listen(Hist) -> 
	receive
		Msg -> 
			H = hash_msg(Msg),
			case lists:member(H, Hist) of
				true -> listen(Hist);
				false -> handle(Msg), listen([H | Hist])
			end
	end.

handle(Msg) ->
	Me = node(),
	case Msg of
		{command, {Node, Module, Function, Args}, _} when Node == Me ->	
			io:fwrite("[~p] Spawning command~n", [node()]),
			spawn(Module, Function, Args);
		_ ->
			io:fwrite("[~p] Forwarding to all~n", [node()]),
			lists:foreach(fun(Target) -> {listener, Target} ! Msg end, nodes())
	end.
