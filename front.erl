-module(front).
-export([start/0]).

start() -> listen().

listen() ->
	Me = node(),
	receive
    	{command, Command, Args} ->
    		spawn(command, Command, Args);
		{command, Node, Command, Args} when Node == Me -> 
    		spawn(command, Command, Args);
		{command, all, Command, Args} ->
			comm:send_to_back({command, all, Command, Args});

		_ -> io:fwrite("Dont understand~n")
	end,
	listen().
