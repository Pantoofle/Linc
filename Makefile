NB_AGENT ?= 10

.SILENT:

ctrl:
	erl -sname ctrl -connect_all false
	
user:
	erl -sname user -connect_all false
	
spawn:
	echo "Spawning the agents : " $(NB_AGENT)
	for i in `seq 1 $(NB_AGENT)`; do \
	echo $$i / $(NB_AGENT); \
	mkdir -p nodes/$$i ; \
	cd nodes/$$i ; \
	erl -sname n$$i -detached -connect_all false; \
	cd ../.. ; \
	done; \
	true

kill:
	killall beam.smp

clean:
	echo "Cleaning..."
	while read f; do \
	if [ "$$f" == "#NO-DEL" ]; then \
	break; \
	fi; \
	echo $$f; \
	rm -fr $$f; \
	done < .gitignore; \
	true
