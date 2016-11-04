
default:
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -linkpkg -o index.byte index.ml
	js_of_ocaml index.byte

clean:
	ocamlbuild -clean
