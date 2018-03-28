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
		{control, Node, Command, Args, _} when Node == Me ->
			spawn(command, Command, Args);

		{control, all, Command, Args, _} ->
			comm:broadcast(Msg),
			spawn(command, Command, Args);
		_ ->
			io:fwrite("[~p] I don't understand~n", [node()]),
			comm:broadcast(Msg)
	end.
