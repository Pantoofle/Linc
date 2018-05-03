-module(storage).
-export([store/3,
		 store_one_part/4, 
		 release/2,
		 read/2,
		 gather/2,
		 balance/0,
		 empty/0,
		 nb_stored_elements/0
		]).


% STORING

store(Id, Parts, Bin) ->
	Chunks = split(Bin, Parts),
	store_parts(Id, Parts, 0, Chunks).
	
store_parts(_, _, _, []) -> ok;
store_parts(Id, Parts, Actual, [Chunk | T]) ->
	store_one_part(Id, Actual, Parts, Chunk),
	store_parts(Id, Parts, Actual+1, T).

store_one_part(Id, Nb, Tot, Bin) ->
	Name = create_name(Id, Nb),
	PartUUID = generate_piece_uuid(Id, Nb),
	io:fwrite("[~p] Storing part ~p (Bin : ~p)~n", 
		[node(), {PartUUID, Id, Nb, Tot}, Bin]),
	case file:write_file(Name, Bin) of
		ok -> ets:insert(linc_files, {PartUUID, Id, Nb, Tot});
		Status ->
			io:fwrite("[~p] Writing file ~p failed: ~p~n", [node(), Name, Status]),
			failed
	end,
	balance().

% DELETING/RELEASING

release(Id, all) ->	
	Files = ets:match_object(linc_files, {'_', Id, '_', '_'}),
	lists:foreach(fun({_UUID, _Id, _Part, _}) -> 
					file:delete(create_name(_Id, _Part)), 
					ets:delete(linc_files, _UUID) 
				  end, 
		      Files);

release(Id, Part) ->
	PartUUID = generate_piece_uuid(Id, Part),
	Files = ets:lookup(linc_files, PartUUID),
	lists:foreach(fun({_UUID, _Id, _Part, _}) -> 
					file:delete(create_name(_Id, _Part)), 
					ets:delete(linc_files, _UUID) 
				  end, 
		      Files).

% When a node is killed, send all its parts to neighbours 
empty() -> empty(ets:match_object(linc_route, {'$1', agent, 1, '_'}) ).

empty([]) -> error;
empty([H | T]) ->
	distribute(ets:first(linc_files), H),
	timer:sleep(1000),
	case nb_stored_elements() of
		0 -> ok;
		_ -> empty(T)
	end.

distribute('$end_of_table', _) -> ok;
distribute(Key, Target) ->
	delegate(Key, Target),
	distribute(ets:next(linc_files, Key), Target).


% READING/ACCESING

read(Id, Part) ->
	file:read_file(create_name(Id, Part)).

gather(Id, Client) ->
	io:fwrite("[~p] Gathering file ~p~n", [node(), Id]),
	Parts = ets:match_object(linc_files, {'_', Id, '_', '_'}),
	lists:foreach(
		fun({_, _Id, _Part, _Tot}) -> 
			case storage:read(_Id, _Part) of
				{ok, Bin} -> 
					comm:send(Client,{file_part, _Id, _Part, _Tot, Bin}),
					io:fwrite("[~p] I found part ~p, sending ~p~n", 
								[node(), _Part,{file_part, _Id, _Part, _Tot}]);
    			_ -> 
					io:fwrite("[~p] I'm suposed to have file ~p (~p). But no such file~n", 
								[node(), _Id, _Part]),
					failed
    		end
		end, 
		Parts).

% BALANCING THE LOAD
balance() ->
	io:fwrite("[~p - balance] Asking for neighbours load~n", [node()]),
	lists:foreach(fun(Node) -> comm:send(Node, {query, Node, {load, node(), self()}}) end, nodes()),
	io:fwrite("[~p - balance] Receiving the results~n", [node()]),
	divide(ets:first(linc_files)).


divide('$end_of_table') -> done;
divide(Key) ->
	MyLoad = nb_stored_elements(),
	receive
		{info, {load, Node, Load}} when Load+1 < MyLoad -> 	
    		io:fwrite("[~p - divide] Delegate ~p to ~p~n", [node(), Key, Node]),
			delegate(Node, Key), 
			divide(ets:next(linc_files, Key));
        {info, {load, Node, Load}} -> 
    		io:fwrite("[~p - divide] Node ~p does not have enough place (Me : ~p | It : ~p)~n", [node(), Node, MyLoad, Load]),
			divide(Key);
		_ -> io:fwrite("[~p - divide] Don't understand~n", [node()]),
			divide(Key)
	after
		6000 -> done
	end.

delegate(Target, Key) ->
	case ets:lookup(linc_files, Key) of
		[{_, Id, Part, Tot} | _] ->	
			case read(Id, Part) of
				{ok, Bin} ->
					comm:send(Target, {command, Target, {store_n_release, [node(), Id, Part, Tot, Bin]}}),
                	io:fwrite("[~p - divide] Send ~p (~p/~p) to ~p ~n", 
						[node(), Id, Part+1, Tot, Target]),
					ok;
				_ -> 
                	io:fwrite("[~p - divide] Could not read file ~p (part ~p)~n",
						 [node(), Id, Part+1]),
					failed
			end;
		[] -> 
        	io:fwrite("[~p - divide] Key ~p not found~n", [node(),Key])
	end.
	

% MISCELANOUS

split(Bin, Parts) ->
	Len = byte_size(Bin),
	Step = (Len div Parts),
	split(Bin, Parts, Step).

split(Bin, 1, _) -> [Bin];
split(Bin, Parts, Step) ->
	Len = byte_size(Bin),
	S = min(Step, Len),
	[binary:part(Bin, 0, S) | split(binary:part(Bin, S, Len-S), Parts-1, Step)].

create_name(Id, PartNb) ->
	string:concat(integer_to_list(Id), string:concat(".part", integer_to_list(PartNb))).

generate_piece_uuid(Id, Part) ->	
	<<UUID:128>> = crypto:hash(md4, 
		list_to_binary([integer_to_list(Id), integer_to_list(Part)])),
	UUID.

nb_stored_elements() ->
	case lists:keysearch(size, 1, ets:info(linc_files)) of
		{value, {size, N}} -> N;
		_ -> error
	end.
