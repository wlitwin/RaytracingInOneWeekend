OCAMLBUILD = ocamlbuild -use-ocamlfind

opt:
	$(OCAMLBUILD) -ocamlc ocamlopt -ocamlopt ocamlopt -cflags -annot,-bin-annot,-noassert,-unbox-closures,-unboxed-types,-unsafe,-nodynlink,-O3,-rounds=10,-unbox-closures-factor=100 -pkg unix -I src src/main.native

dbg:
	$(OCAMLBUILD) -cflags -annot,-bin-annot -pkg unix -I src src/main.d.byte

prof:
	$(OCAMLBUILD) -cflags -annot,-bin-annot -pkg unix -I src src/main.p.native

perf:
	sudo perf record -g --call-graph=dwarf ./main.native 0

report:
	sudo perf report -G --tui

test: all
	$(OCAMLBUILD) -pkg oUnit -Is src,test test/test.native
	./test.native

clean:
	$(OCAMLBUILD) -clean
