-module(back).
-export([start/0]).

start() ->
	% Initiate the local map of the topology
	case lists:member(linc_route, ets:all()) of
		true  -> ok;
		false -> ets:new(linc_route, [set, public, named_table])
	end,
	lists:foreach(fun(Node) ->ets:insert(linc_route, {Node, 1, Node}) end, nodes()),
	listen().

listen() -> listen([]).
listen(Hist) -> 
	% When listeneng, we test if the message is not already seen. If it is the case, dont handle it
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
        % ROUTING FUNCTIONS
		{path, From, To, Dist} -> update_path(From, To, Dist);
		{whereis, Target} -> find(Target, Seed);
		{link, Node} -> add_link(Node);
		{dead, Node} -> ets:delete(linc_route, Node);
		{update} -> update_direct_link();

		% COMMAND FUNCTIONS, THEY ARE HANDLED BY THE FRONTEND
		{command, Node, Command, Args} when Node == Me -> comm:send_to_front({comm, Command, Args});
		{command, all, Command, Args} ->
			comm:forward_all({Msg, Seed}),
            comm:send_to_front({command, Command, Args});

		_ ->
			io:fwrite("Nope, don't recognize~n"),
			comm:forward_all({Msg, Seed})
	end.

update_path(From, To, Dist) ->
	case ets:lookup(linc_route, To) of
		[]    -> io:fwrite("Adding new destination: ~p ~n", [To]),
			     ets:insert(linc_route, {To, Dist+1, From}),	
				 comm:broadcast({path, node(), To, Dist+1});
		[{_, _Dist, _By}|_]  -> 
			case Dist+1 < _Dist of
				true  -> io:fwrite("Updating dist to ~p~n", [To]),
						 ets:insert(linc_route, {To, Dist+1, From}),
						 comm:broadcast({path, node(), To, Dist+1}),
						 better;
				false -> not_better
            end
	end.

find(Target, Seed) -> 
	case ets:lookup(linc_route, Target) of
		[] -> comm:forward({{whereis, Target}, Seed});
		[{_To, _Dist, _From}|_] -> comm:broadcast({path, node(), _To, _Dist})
	end.

add_link(Node) ->
	case net_adm:ping(Node) of
		pong ->	ets:insert(linc_route, {Node, 1, Node});
		pang -> fail
	end.

update_direct_link() ->
	io:fwrite("[~p] Router : Updating the neighbours~n", [node()]),
	% Delete the old links of size 1 : The direct neighbours
	ets:match_delete(linc_route, {"_", "1","_"}),
	% Add the new direct neighbours
	lists:foreach(fun(Node) -> ets:insert(linc_route, {Node, 1, Node}) end, nodes()).

