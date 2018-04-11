-module(agent).
-export([listen/0, 
		 handle/2]).

listen() -> listen([]).
listen(Hist) -> 
	receive
		{Data, Seed} -> 
			H = comm:hash({Data, Seed}),
			case lists:member(H, Hist) of
				true  -> listen(Hist);
				false -> handle(Data, Seed), listen([H | Hist])
			end
	end.	

handle(Msg, Seed) ->
	Me = node(),
	case Msg of
		{command, Node, Command, Args} when Node == Me ->
			spawn(command, Command, Args);

		{command, all, Command, Args} ->
			comm:forward_all({Msg, Seed}),
			spawn(command, Command, Args);

		_ ->
			comm:forward_all({Msg, Seed})
	end.
