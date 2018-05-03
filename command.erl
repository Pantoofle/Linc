-module(command).
-export([shutdown/0,
		 neighbours/1,
		 load/1,
		 link/1,
		 recover/2,
		 gather/2,
		 store/3,
		 release/2,
		 balance/0,
		 store_n_release/5
        ]).

shutdown() ->
	storage:empty(),
	lists:foreach(fun(Node) -> comm:send(Node, {dead, node()}) end, nodes()),
	erlang:exit(whereis(front), exit),
	erlang:exit(whereis(back), exit).

% Client asked for this node's neighborhood
neighbours(Client) -> 
	comm:send(Client, {{display, neighbours}, Client, {node(), nodes()}}).

% Client asked for this node's load
load(Client) -> 
	comm:send(Client, {{display, load}, Client, {node(), storage:nb_stored_elements()}}),
	io:fwrite("[~p] The data I hold : ~n~p~n", [node(), ets:match_object(linc_files, '_')]).

link(Node) ->
	comm:send(Node, {query_link, agent, node()}).

% DATA STORAGE

% Store a file on this node
store(Id, Parts, Bin) ->
	storage:store(Id, Parts, Bin).

% A user, Client,  asked this node to recover the file Id.
% We call the gather function on all nodes, they will send their parts of the file
recover(Id, Client) ->
	comm:send_to_back({command, all, {gather, [Id, Client]}}).

% Send to Client all the parts of file Id stored in this node
gather(Id, Client) ->
	storage:gather(Id, Client).

% Release a piece of data

release(Id, Part) ->
	storage:release(Id, Part).

% Balance the load
balance() ->
	storage:balance().


% When moving parts to equilibrate
store_n_release(Node, Id, Part, Tot, Bin) ->
	io:fwrite("[~p] Storing part ~p of Id ~p~n", [node(),Part, Id]),
	storage:store_one_part(Id, Part, Tot, Bin),
	io:fwrite("[~p] Asking ~p to release its parts~n", [node(), Node]),
	comm:send(Node, {command, Node, {release, [Id, Part]}}),
	balance().

