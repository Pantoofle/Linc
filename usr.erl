-module(usr).
-export([setup/0,
		 link/1,
		 listen/0,
		 sendCommand/4,
		 sendCommand/3,
		 sendCommand/2,
		 sendCommand/1,
		 shutdown/1, 
		 neighbours/0,
		 load/0,
		 store_file/2,
		 recover_file/2,
		 release_file/1
		]).

% SETUP COMMANDS 
setup() ->
	Modules = [comm,
			   storage],
 
	lists:foreach(fun(Mod) -> c:c(Mod) end, Modules),
	io:fwrite("[usr] Spawning the listener~n"),
	case lists:member(back, registered()) of
		true -> unregister(back), ok;
		false-> ok
	end,
	case lists:member(front, registered()) of
		true -> unregister(front), ok;
		false-> ok
	end,

	case lists:member(linc_route, ets:all()) of
		true  -> ok;
		false -> ets:new(linc_route, [set, public, named_table])
	end,
	lists:foreach(fun(Node) -> comm:send(Node, {query_link, node(), user}) end, nodes()),
	ets:insert(linc_route, {node(), user, 0, node()}),

	register(back, spawn(?MODULE, listen, [])),
	register(front, self()),
	io:fwrite("[usr] Initializated. Ready to receive commands~n").

link(Entry) -> 
	case net_adm:ping(Entry) of
		pong -> 
			comm:send(Entry, {query_link, user, node()});
		pang -> io:fwrite("[usr] Failed to reach node ~p~n", [Entry])
	end.


% OUTPUT COMMANDS, DISPLAY THE RESULTS OF THE QUERIES
listen() -> 
	receive 
		{Msg, _} -> handle(Msg);
		_ -> io:fwrite("[usr] Received wrong format~n")
	end,
	listen().

handle(Msg) ->
	case Msg of
		% The display functions. When asked for informations
		{{display, Tag}, Target, {From, Data}} when Target == node() ->
			io:fwrite("[~p] My ~p : ~p~n", [From, Tag, Data]);
		
		% Answer to pings. Tell them you are a user node, dont receive.
		{query_link, Node} -> 
			ets:insert(linc_route, {Node, agent, 1, Node}),
			comm:send(Node, {user_node, node()}),
			ets:delete(linc_route, {Node, agent, 1, Node}),
			erlang:disconnect(Node);

		{confirm_link, Node} ->
		    io:fwrite("[usr] Linked to ~p with success~n", [Node]),
			ets:insert(linc_route, {Node, agent, 1, Node});

		_ -> {front, node()} ! Msg
	end.



% SEND A SPECIFIC COMMAND
sendCommand(Entry, Target, Command, Args) ->
	io:fwrite("[usr] Sending to front of entry point ~p the msg :~p~n", 
		[Entry, {command, Target, {Command, Args}}]),
	{front, Entry} ! {command, Target, {Command, Args}}.

sendCommand(Target, Command, Args) ->
	sendCommand(lists:last(nodes()), Target, Command, Args).
	
sendCommand(Command, Args) ->
	N = lists:last(nodes()),
	sendCommand(N, N, Command, Args).

sendCommand(Command) ->
	sendCommand(Command, []).


% NODE COMMANDS
shutdown(Node) ->
	sendCommand(Node, shutdown, []).

neighbours() ->
	sendCommand(all, neighbours, [node()]).

load() ->
	sendCommand(all, load, [node()]).


% FILE STORAGE FUNCTIONS
store_file(Filename, Parts) ->
	case file:read_file(Filename) of
		{error, Reason} -> io:fwrite("[usr] Read File failed: ~p~n", [Reason]), fail;
		{ok, Bin} ->
			<<Id:128>> = crypto:hash(md4, 
							list_to_binary([Bin, integer_to_list(rand:uniform(4096))])),
			sendCommand(store, [Id, Parts, Bin]),
			{ok, Id}
	end.

recover_file(Id, File) ->
	io:fwrite("[usr] Gathering the pieces of the file...~n"),
	sendCommand(recover, [Id, node()]),
	% Now we receive the pieces from the nodes
	Chunks = receive_parts(Id),
	case Chunks of
		failed -> io:fwrite("[usr] Failed to receive the file~n");
		_ ->
			io:fwrite("[usr] Received file. Saving it to ~p~n", [File]),
        	file:write_file(File, binary:list_to_bin(array:to_list(Chunks)))
	end.

receive_parts(Id)->	
	% Receive the first part, this sets the size of the array
	receive
		{file_part, _Id, _Part, _Tot, _Bin} when Id == _Id -> 
			io:fwrite("[usr] Received meta data~n"),
    		Chunks = array:resize(_Tot, array:new()),
			io:fwrite("[usr] Received part 1 / ~p~n", [_Tot]),
			receive_parts(Id, array:set(_Part, _Bin, Chunks), _Tot-1);
		_ -> receive_parts(Id)
	after 
		10000 -> 
			io:fwrite("[usr] No node seem to have this file~n"), 
			failed
	end.

receive_parts(_, Chunks, 0) -> Chunks;
receive_parts(Id, Chunks, Remain) -> 
	receive
		{file_part, _Id, _Part, _Tot, _Bin} when Id == _Id -> 
			case array:get(_Part, Chunks) == array:default(Chunks) of
				true ->						
					io:fwrite("[usr] Received part ~p / ~p (~p bytes)~n", 
								[_Tot-Remain+1, _Tot, byte_size(_Bin)]),
					receive_parts(Id, array:set(_Part, _Bin, Chunks), Remain-1);
				false -> receive_parts(Id, Chunks, Remain)
			end;
		_ -> io:fwrite("[usr] This is not a file part... skipping~n"), receive_parts(Id, Chunks, Remain)

	after 
		10000 -> 
			io:fwrite("[usr] No part received for 10s and still missing some parts. Maybe a piece of the file was lost~n"),
			failed
    end.

release_file(Id) ->
	io:fwrite("[usr] Releasing the file~n"),
	sendCommand(release, [Id, '_']).
