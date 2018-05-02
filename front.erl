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
    	{command, {Command, Args}} ->
    		spawn(command, Command, Args);

		% RECEIVED FROM A USER, DETERMINE IF EXECUTE IMEDIATELY OR SEND TO BACK BEFORE
		{command, Node, {Command, Args}} when Node == Me -> 
    		spawn(command, Command, Args);
		{command, Node, {Command, Args}} ->
			io:fwrite("[~p - front] This command belongs to back~n", [node()]),
			comm:send_to_back({command, Node, {Command, Args}});
		
		% SOMEONE ASKED FOR INFO
		{query, {load, Node, Process}} ->
			comm:send(Node, {{info, Process}, Node, {load, node(), storage:nb_stored_elements()}});

		% SOMEONE GAVE INFO TO PROCESS Target
		{{info, Target} , Data} ->
			io:fwrite("[~p] Sending ~p to process ~p~n", [node(), Data, Target]),
            Target ! {info, Data};

		A -> io:fwrite("[~p] Dont understand : ~p~n", [node(), A])
	end,
	listen().
