-module(routing).
-export([
	update_path/4,
	find/2,
	query_link/2,
	add_link/2,
	update_direct_link/0,
    check_old_messages/1
	]).

update_path(From, To, Type, Dist) ->
	case ets:lookup(linc_route, To) of
		[]    -> ets:insert(linc_route, {To, Type, Dist+1, From}),	
				 comm:broadcast({path, node(), To, Dist+1}),
				 check_old_messages(To);
		[{_, _, _Dist, _By}|_]  -> 
			case Dist+1 < _Dist of
				true  -> ets:insert(linc_route, {To, Type, Dist+1, From}),
						 comm:broadcast({path, node(), To, Type, Dist+1}),
        				 check_old_messages(To),
						 better;
				false -> not_better
            end
	end.

find(Target, Seed) -> 
	case ets:lookup(linc_route, Target) of
		[] -> comm:forward_all({{whereis, Target}, Seed});
		[{_To, _Type,  _Dist, _}|_] -> 
			comm:broadcast({path, node(), _To, _Type, _Dist})
	end.

query_link(Node, Type) ->
	add_link(Node, Type),
	comm:send(Node, {confirm_link, node()}).

add_link(Node, Type) ->
	io:fwrite("[~p] Added link to ~p~n", [node(), Node]),
	case net_adm:ping(Node) of
		pong ->	
			ets:insert(linc_route, {Node, Type, 1, Node}), 
			check_old_messages(Node);
		pang -> fail
	end.

update_direct_link() ->
	% Delete the old links of size 1 : The direct neighbours
	ets:match_delete(linc_route, {"_", "_", "1","_"}),
	% Ask again the nodes
	lists:foreach(fun(Node) -> comm:send(Node, {query_link, agent, node()}) end, nodes()).

check_old_messages(Target) ->
	case ets:lookup(linc_wait, Target) of
		[] -> nothing;
		[{_, Msg}| _] -> comm:send(Target, Msg )
	end.
	
