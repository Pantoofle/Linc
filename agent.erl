-module(agent).
-export([listen/0, 
		 handle/1]).

listen() -> listen([]).
listen(Hist) -> 
	receive
		Msg -> 
			H = comm:hash(Msg),
			case lists:member(H, Hist) of
				true -> listen(Hist);
				false -> handle(Msg), listen([H | Hist])
			end
	end.	

handle(Msg) ->
	Me = node(),
	case Msg of
		{command, Node, {Module, Function, Args}, _} when Node == Me ->	
			spawn(Module, Function, Args);

		{control, Node, Command} when Node == Me ->
			command:Command();

		{control, all, Command} ->
			comm:broadcast(Msg),
			command:Command();

		_ ->
			comm:broadcast(Msg)
	end.
