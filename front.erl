-module(front).
-export([start/0]).

start() -> 
	case lists:member(linc_files, ets:all()) of
		true  -> ok;
		false -> ets:new(linc_files, [bag, public, named_table])
	end,
    listen().

listen() ->
	Me = node(),
	receive
		% RECEIVED FROM THE BACK, IT ASKS FOR A FUNCTION CALL
    	{command, Command, Args} ->
    		spawn(command, Command, Args);

		% RECEIVED FROM A USER, DETERMINE IF EXECUTE IMEDIATELY OR SEND TO BACK BEFORE
		{command, Node, Command, Args} when Node == Me -> 
    		spawn(command, Command, Args);
		{command, all, Command, Args} ->
			comm:send_to_back({command, all, Command, Args});

		_ -> io:fwrite("Dont understand~n")
	end,
	listen().
