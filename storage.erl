-module(storage).
-export([store/3, 
		 release/2,
		 read/2]).


% STORING

store(Id, Parts, Bin) ->
	Chunks = split(Bin, Parts),
	store_parts(Id, Parts, 0, Chunks).
	
store_parts(_, _, _, []) -> ok;
store_parts(Id, Parts, Actual, [Chunk | T]) ->
	Name = create_name(Id, Actual),
	file:write_file(Name, Chunk),
	ets:insert(linc_files, {Id, Actual, Parts}),
	store_parts(Id, Parts, Actual+1, T).


% DELETING/RELEASING

release(Id, Part) ->
	Files = ets:match_object(linc_files, {Id, Part, '_'}),
	lists:map(fun({_Id, _Part, _Tot}) -> file:delete(create_name(_Id, _Part)), 
								      ets:delete_object(linc_files, {_Id, _Part, _Tot}) end, 
		      Files).

% READING/ACCESING
read(Id, Part) ->
	file:read_file(create_name(Id, Part)).

% MISCELANOUS

split(Bin, Parts) ->
	Len = byte_size(Bin),
	Step = ((Len -1) div Parts) +1,
	Pos = lists:seq(0, Len-1, Step),
	lists:map( fun(Start) -> binary:part(Bin, Start, min(Step, Len - Start)) end, Pos).

create_name(Id, PartNb) ->
	string:concat(integer_to_list(Id), string:concat(".part", integer_to_list(PartNb))).
