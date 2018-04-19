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
		 store_file/2,
		 recover_file/2 
		]).

% SETUP COMMANDS 
setup() ->
	io:fwrite("[usr] Spawning the listener~n"),
	case lists:member(back, registered()) of
		true -> unregister(back), ok;
		false-> ok
	end,
	register(back, spawn(?MODULE, listen, [])),
	register(front, self()),
	io:fwrite("[usr] Initializated. Ready to receive commands~n").

link(Entry) -> 
	case net_adm:ping(Entry) of
		pong -> {back, Entry} ! {{link, node()}, self()}, 
			    io:fwrite("[usr] Linked to ~p with success~n", [Entry]);
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
		{neighbours, From, Neighbours} -> io:fwrite("[~p] My neighbours : ~p~n", [From, Neighbours]);
		_ -> {front, node()} ! Msg
	end.



% SEND A SPECIFIC COMMAND
sendCommand(Entry, Target, Command, Args) ->
	io:fwrite("[usr] Sending command to entry point ~p~n", [Entry]),
	{front, Entry} ! {command, Target, Command, Args}.

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


% FILE STORAGE FUNCTIONS
store_file(Filename, Parts) ->
	case file:read_file(Filename) of
		{error, Reason} -> io:fwrite("[usr] Read File failed: ~p~n", [Reason]), fail;
		{ok, Bin} ->
			<<Id:128>> = crypto:hash(md4, Bin),
			sendCommand(store, [Id, Parts, Bin]),
			{ok, Id}
	end.

recover_file(Id, File) ->
	io:fwrite("[usr] Gathering the pieces of the file...~n"),
	sendCommand(recover, [Id, node()]),
	% Now we receive the pieces from the nodes
	Chunks = receive_parts(Id),
	io:fwrite("[usr] Received file. Saving it to ~p~n", [File]),
	file:write_file(File, binary:list_to_bin(array:to_list(Chunks))).


receive_parts(Id)->	
	% Receive the first part, this sets the size of the array
	Chunks = array:new(),
	receive
		{file_part, _Id, _Part, _Tot, _Bin} when Id == _Id -> 
			io:fwrite("[usr] Received meta data~n"),
    		array:resize(_Tot, Chunks),
			array:set(_Part, _Bin, Chunks),
			io:fwrite("[usr] Received part 1 / ~p~n", [_Tot]),
			receive_parts(Id, Chunks, _Tot-1);
		_ -> receive_parts(Id)
	end.

receive_parts(_, Chunks, 0) -> Chunks;
receive_parts(Id, Chunks, Remain) -> 
	receive
		{file_part, _Id, _Part, _Tot, _Bin} when Id == _Id -> 
			case array:get(_Part, Chunks) == array:default(Chunks) of
				true ->	
					array:set(_Part, _Bin, Chunks), 
					io:fwrite("[usr] Received part ~p / ~p (~p bytes)~n", [_Tot-Remain+1, _Tot, byte_size(_Bin)]),
					receive_parts(Id, Chunks, Remain-1);
				false -> receive_parts(Id, Chunks, Remain)
			end;
		_ -> io:fwrite("[usr] This is not a file part... skipping~n"), receive_parts(Id, Chunks, Remain)
    end.
