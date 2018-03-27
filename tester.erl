-module(tester).
-export([sendCommand/3, sendMsg/3]).

sendCommand(Entry, Target, {Module, Fun, Args}) ->
	{listener, Entry} ! {command, {Target, Module, Fun, Args}, rand:uniform(4096)}.

sendMsg(Entry, Target, Msg) ->
	{listener, Entry} ! {command, {Target, io, fwrite, [Msg, []]}, rand:uniform(4096)}.

