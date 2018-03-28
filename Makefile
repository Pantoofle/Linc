.SILENT:
clean:
	echo "Cleaning..."
	while read f; do \
	if [ "$$f" == "#NO-DEL" ]; then \
	break; \
	fi; \
	rm -f $$f; \
	done < .gitignore; \
	true
