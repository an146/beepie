B=_build
COMMON=dirs

all: $B/gui

$B/gui: ${COMMON}
	ocamlfind ocamlopt -o $@ -I +lablgtk2 lablgtk.cmxa gtkInit.cmx gui/gui.ml

dirs:
	mkdir -p $B

clean:
	rm -Rf $B

.PHONY: all dirs clean
