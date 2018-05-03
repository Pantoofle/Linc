-module(back).
-export([start/0]).

start() ->
	% Initiate the local map of the topology
	case lists:member(linc_route, ets:all()) of
		true  -> ok;
		false -> ets:new(linc_route, [set, public, named_table])
	end,
	lists:foreach(fun(Node) -> comm:send(Node, {query_link, node(), agent}) end, nodes()),
	ets:insert(linc_route, {node(), agent, 0, node()}),

	% Initiate the wait queue
	case lists:member(linc_wait, ets:all()) of
		true  -> ok;
		false -> ets:new(linc_wait, [bag, public, named_table])
	end,
	
	% Initiate the acknowlegment queue
	case lists:member(linc_ack, ets:all()) of
		true  -> ok;
		false -> ets:new(linc_ack, [set, public, named_table])
	end,
    listen().

listen() -> listen([]).
listen(Hist) -> 
	% When listening, we test if the message is not already seen. If it is the case, dont handle it
	receive
		{Data, Seed, Source} -> 
			% Send ack if needed
			comm:ack({Data, Seed, Source}),
			H = comm:hash({Data, Seed}),
			case lists:member(H, Hist) of
				true  -> listen(Hist);
				false -> handle(Data, Seed), listen([H | Hist])
			end
		after 
			500 -> comm:refresh_ack(), listen(Hist)
	end.	

handle(Msg, Seed) ->
	Me = node(),
	case Msg of
        % ROUTING FUNCTIONS
		{path, From, To, Type, Dist} -> routing:update_path(From, To, Type, Dist);
		{whereis, Target} -> routing:find(Target, Seed);
		{query_link, Type, Node} -> routing:query_link(Node, Type);
		{confirm_link, Node} -> routing:add_link(Node, agent);
		{link, Node} -> comm:send(Node, {query_link, agent, node()});
		{user_node, _} -> failed;
		{dead, Node} -> ets:delete(linc_route, Node);
		{update} -> routing:update_direct_link();

		% COMMAND FUNCTIONS, THEY ARE HANDLED BY THE FRONTEND
		{Tag, Target, Data} when Target == Me -> 
			comm:send_to_front({Tag, Data});
		{Tag, all, Data} ->
			comm:forward_all({Msg, Seed, node()}),
            comm:send_to_front({Tag, Data});		
		{_, Target, _} -> comm:send(Target, Msg); 

		% THE ACK PROTOCOL
		{ack, Id} -> comm:receive_ack(Id);

		% DEFAULT BEHAVIOUR WHEN DONT UNDERSTAND: MAYBE ANOTHER NODE CAN UNDERSTAND THIS
		_ ->
			io:fwrite("[~p] Don't understand this ~p. Forwarding~n", 
				[node(), Msg]),
			comm:forward_all({Msg, Seed, node()})
	end.
