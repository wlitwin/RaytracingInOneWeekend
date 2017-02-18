OCAMLBUILD = ocamlbuild -use-ocamlfind

opt:
	$(OCAMLBUILD) -I src src/main.native

dbg:
	$(OCAMLBUILD) -cflags -annot,-bin-annot -I src src/main.d.byte

test: all
	$(OCAMLBUILD) -pkg oUnit -Is src,test test/test.native
	./test.native

clean:
	$(OCAMLBUILD) -clean
