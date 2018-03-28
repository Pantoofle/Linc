#! /bin/sh 

echo Spawning the agents
for i in $(seq 1 $1); do
	echo $i / $1
	erl -sname n$i -detached -connect_all false
done


