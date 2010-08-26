B=_build
COMMON=dirs

all: $B/gui

test: $B/test
	$B/test

$B/gui: ${COMMON}
	ocamlfind ocamlopt -o $@ -I +lablgtk2 lablgtk.cmxa gtkInit.cmx gui/gui.ml

$B/test: ${COMMON}
	ocamlfind ocamlopt -package oUnit -linkpkg -o $@ tests/add.ml

dirs:
	mkdir -p $B

clean:
	rm -Rf $B

.PHONY: all dirs clean
