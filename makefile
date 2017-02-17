OCAMLBUILD = ocamlbuild -use-ocamlfind

all:
	$(OCAMLBUILD) -use-menhir -cflags -annot,-bin-annot -pkg lwt.glib,lwt,cairo2,cairo2.lablgtk2,lablgtk2,lablgtk2-extras,ocamlgraph -I src src/main.d.byte

test: all
	$(OCAMLBUILD) -pkg oUnit -Is src,test test/test.native
	./test.native

clean:
	$(OCAMLBUILD) -clean
