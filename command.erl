-module(command).
-export([shutdown/0,
		 neighbours/1,
		 link/1,
		 recover/2,
		 gather/2,
		 store/3]).

shutdown() ->
	lists:foreach(fun(Node) -> comm:send(Node, {dead, node()}) end, nodes()),
	exit(front, shutdown),
	exit(back, shutdown).

% Client asked for this node's neighborhood
neighbours(Client) -> 
	comm:send(Client, {neighbours, node(), nodes()}).

link(Node) ->
	comm:send_to_back({link, Node}).

% Store a piece of a file on this node
store(Id, Parts, Bin) ->
	storage:store(Id, Parts, Bin).

% A user, Client,  asked this node to recover the file Id.
% We call the gather function on all nodes, they will send their parts of the file
recover(Id, Client) ->
	comm:send_to_back({command, all, gather, [Id, Client]}).

% Send to Client all the parts of file Id stored in this node
gather(Id, Client) ->
	Parts = ets:match_object(linc_files, {Id, '_', '_'}),
	lists:map(
		fun({_Id, _Part, _Tot}) -> 
			case storage:read(_Id, _Part) of
				{ok, Bin} -> comm:send(Client,{file_part, _Id, _Part, _Tot, Bin});
    			_ -> failed
    		end
		end, 
		Parts).



