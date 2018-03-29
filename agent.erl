-module(agent).
-export([listen/0, 
		 handle/1]).

listen() -> listen([]).
listen(Hist) -> 
	receive
		{Data, Seed} -> 
			H = comm:hash({Data, Seed}),
			case lists:member(H, Hist) of
				true -> listen(Hist);
				false -> handle(Data), listen([H | Hist])
			end
	end.	

handle(Msg) ->
	Me = node(),
	case Msg of
		{command, Node, Command, Args} when Node == Me ->
			spawn(command, Command, Args);

		{command, all, Command, Args} ->
			comm:broadcast(Msg),
			spawn(command, Command, Args);
		_ ->
			io:fwrite("[~p] I don't understand~n", [node()]),
			comm:broadcast(Msg)
	end.
