CAMLOPT = ocamlopt
LIBS = unix.cmxa threads.cmxa graphics.cmxa

life:
	$(CAMLOPT) -thread $(LIBS) -o life main.ml

clean: 
	rm main.cmi main.cmx main.o life
