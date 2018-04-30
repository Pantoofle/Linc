-module(comm).
-export([hash/1, 
		 broadcast/1,
		 send/2, 
		 forward_all/1,
		 send_to_back/1,
		 send_to_front/1]).

hash(Msg) ->
	S = io_lib:format("~p", [Msg]),
	crypto:hash(md5, lists:flatten(S)).

broadcast(Msg) ->
	send(all, Msg).

forward_all(Msg) ->
	lists:foreach(fun(Target) -> {back, Target} ! Msg,
							io:fwrite("[~p] Sending ~p to ~p~n", [node(), Msg, Target])
					end, nodes()).

send(all, Msg) ->
	Rnd = rand:uniform(4096),
	lists:foreach(fun(Node) -> {back, Node} ! {Msg, Rnd},
                			io:fwrite("[~p] Sending ~p to ~p~n", [node(), Msg, Node])
            	 end, nodes());

send(Node, Msg) ->
	case ets:lookup(linc_route, Node) of
		[] -> 
        	io:fwrite("[~p] Don't know where ~p is. Asking my neighbours~n", [node(), Node]),
            send(all, {whereis, Node}), ets:insert(linc_wait, {Node, Msg}), ok;
		[{_, _, By} | _] -> 
        	io:fwrite("[~p] Sending ~p to ~p via ~p~n", [node(), Msg, Node, By]),
    		{back, By} ! {Msg, rand:uniform(4096)}, ok
	end.

send_to_front(Msg) -> 
	{front, node()} ! Msg.

send_to_back(Msg) -> 
	send(node(), Msg).
