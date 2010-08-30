# fixes ocamlfind unable to find lablgtk2
[ `whoami` != "root" ] && {
	echo please exec as root
	exit 1
}

mkdir /usr/lib/ocaml/site-lib/lablgtk2

cat > /usr/lib/ocaml/site-lib/lablgtk2/META <<END
requires=""
version="2.12.0"
archive(byte)="lablgtk.cma"
archive(byte,init)="lablgtk.cma gtkInit.cmo"
archive(native)="lablgtk.cmxa"
archive(native,init)="lablgtk.cmxa gtkInit.cmx"
linkopts=""
directory="+lablgtk2"
END
