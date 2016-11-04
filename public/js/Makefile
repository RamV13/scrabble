
#all:
#	ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml.syntax -syntax camlp4o -linkpkg -o index.byte index.ml
#	js_of_ocaml index.byte

OBJS=httpclient.cmo
NAME=index
OFIND=ocamlfind ocamlc -thread -package js_of_ocaml,js_of_ocaml.syntax,lwt.ppx,cohttp.js -syntax camlp4o

$(NAME).byte: $(OBJS)
		$(OFIND) -linkpkg -o $@ $(OBJS) $(NAME).ml
		js_of_ocaml $@

%.cmo: %.ml
		$(OFIND) -c $<i
		$(OFIND) -c $<

clean:
	rm *.cm*
	rm *.byte
	rm $(NAME).js
