-module(comm).
-export([hash/1, 
		 broadcast/1,
		 send/2, 
		 forward_all/1,
		 send_to_back/1,
		 send_to_front/1,
		 receive_ack/1,
		 ack/2,
		 refresh_ack/0
]).

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
	lists:foreach(fun(Node) -> {back, Node} ! {Msg, Rnd} end, nodes());

send(Node, Msg) ->
	% io:fwrite("[~p] Try to send ~p to ~p~n", [node(), Msg, Node]),	
	case Msg of 
		{query_link, _, _} ->
        	{back, Node} ! {Msg, rand:uniform(4096)};

		_ ->
        	case ets:lookup(linc_route, Node) of
        		[] -> 
  					% io:fwrite("[~p] Don't know where ~p is. Asking my neighbours~n", [node(), Node]),
                    send(all, {whereis, Node}), 
        			ets:insert(linc_wait, {Node, Msg}), 
        			ok;
        		[{_, _, _, By} | _] -> 
                	% io:fwrite("[~p] Sending ~p to ~p via ~p~n", [node(), Msg, Node, By]),
            		{back, By} ! {Msg, rand:uniform(4096)}, 
        			ok
        	end
	end.

send_to_front(Msg) -> 
	{front, node()} ! Msg.

send_to_back(Msg) -> 
	send(node(), Msg).

% THE ACK FUNCTIONS

% When received an Ack
receive_ack(Id) ->
	ets:delete(linc_ack, Id).

% Send an Ack
ack(Target, Msg) ->
	Id = hash(Msg),
	send(Target, {ack, Id}).

refresh_ack() ->
    resend(ets:first(linc_ack)).

resend('$end_of_table') -> ok;
resend(Key) ->
	case ets:lookup(linc_ack, Key) of
		[] -> ok;
		[{_, Msg, Target}] -> {back, Target} ! Msg
	end,
	resend(ets:next(linc_ack, Key)).




