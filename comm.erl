-module(comm).
-export([hash/1, 
		 broadcast/1,
		 send/2, 
		 send/3,
		 forward_all/1,
		 send_to_back/1,
		 send_to_front/1,
		 receive_ack/1,
		 ack/1,
		 refresh_ack/0]).

hash(Msg) ->
	S = io_lib:format("~p", [Msg]),
	crypto:hash(md5, lists:flatten(S)).

broadcast(Msg) ->
	send(all, Msg).

forward_all(Msg) ->	
	% io:fwrite("[~p] Forwarding ~p to all neighbours (~p)~n", [node(), Msg, nodes()]),
	lists:foreach(fun(Target) -> {back, Target} ! Msg end, nodes()).

send(all, Msg) ->
	Rnd = rand:uniform(4096),
	% io:fwrite("[~p] Sending ~p to all neighbours (~p)~n", [node(), Msg, nodes()]),
	lists:foreach(fun(Node) -> {back, Node} ! {Msg, Rnd, node()} end, nodes());

send(Node, Msg) -> send(Node, Msg, rand:uniform(4096) ).
send(Node, Msg, Seed) ->
	% io:fwrite("[~p] Try to send ~p to ~p~n", [node(), Msg, Node]),	
	case Msg of 
		{query_link, _, _} ->
        	{back, Node} ! {Msg, Seed, node()};

		_ ->
        	case ets:lookup(linc_route, Node) of
        		[] -> 
  					% io:fwrite("[~p] Don't know where ~p is. Asking my neighbours~n", [node(), Node]),
                    send(all, {whereis, Node}), 
        			ets:insert(linc_wait, {Node, Msg}), 
        			ok;
        		[{_, _, _, By} | _] -> 
                	% io:fwrite("[~p] Sending ~p to ~p via ~p~n", [node(), Msg, Node, By]),
					% Check if we are waiting for an ack
                	case Msg of
                		{command, _, {Command, _}} ->
                			case lists:member( Command, commands_to_ack()) of
                				true -> add_ack({Msg, Seed, node()}, By), ok;
								_ -> ok
							end;
						_ -> ok
					end, 
            		{back, By} ! {Msg, Seed, node()}, 
        			ok
        	end
	end.

send_to_front(Msg) -> 
	{front, node()} ! Msg.

send_to_back(Msg) -> 
	send(node(), Msg).

% THE ACK FUNCTIONS
commands_to_ack() -> [shutdown, link, recover, gather, store, store_n_release].


% Add an ack to the wait list
add_ack(Msg, Target) ->
	Id = hash(Msg),
	ets:insert(linc_ack, {Id, Msg, Target}).

% When received an Ack
receive_ack(Id) ->
	ets:delete(linc_ack, Id).

% Send an Ack if needed
ack(Msg) ->
    case Msg of
    	{{command, _, {Command, _}}, _, Source} ->
    		case lists:member(Command, commands_to_ack()) of
    			true ->
					Id = hash(Msg),
					io:fwrite("[~p] Ack ~p to ~p~n", [node(), Id, Source]),
                	send(Source, {ack, Id});
    			_ -> ok
    		end;
    	_ -> ok
    end.

refresh_ack() ->
    resend(ets:first(linc_ack)).

resend('$end_of_table') -> ok;
resend(Key) ->
	case ets:lookup(linc_ack, Key) of
		[] -> ok;
		[{_, {Msg, Seed, _}, Target}] -> 
			io:fwrite("[~p] ~p did not ack my previous message sending it again~n", 
				[node(), Target]),	
			routing:shrink(),
			ets:insert(linc_wait, {Target, Msg, Seed})
	end,
	resend(ets:next(linc_ack, Key)).




