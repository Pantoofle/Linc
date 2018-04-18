-module(usr).
-export([sendCommand/4,
		 sendCommand/3,
		 sendCommand/2,
		 sendCommand/1,
		 shutdown/1, 
		 neighbours/0]).

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
	sendCommand(all, neighbours, []).
