
all:
	ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o -linkpkg -o index.byte index.ml
	js_of_ocaml index.byte

clean:
	ocamlbuild -clean
