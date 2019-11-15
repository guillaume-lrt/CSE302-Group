DUNE := dune

.PHONY: all
all:
	dune build main.exe
	if test -x _build/default/main.exe ; then \
	  rm -f interpret.exe ; \
	  ln -s _build/default/main.exe interpret.exe ; \
	fi

.PHONY: clean
clean:
	dune clean
	rm -f interpret.exe
	rm -f test/*.c test/*.exe
