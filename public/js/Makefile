
OBJS=grid.cmo game.cmo xHRClient.cmo scrabbleClient.cmo
NAME=index
GUI=scrabble
OFIND=ocamlfind ocamlc -thread -package js_of_ocaml,js_of_ocaml.syntax,cohttp.lwt,cohttp.async,lwt.ppx,cohttp.js,yojson -syntax camlp4o

$(NAME).byte: $(OBJS)
		$(OFIND) -linkpkg -o $@ $(OBJS) $(NAME).ml
		js_of_ocaml +nat.js $@
		$(OFIND) -linkpkg -o $(GUI).byte $(OBJS) $(GUI).ml
		js_of_ocaml +nat.js $(GUI).byte

%.cmo: %.ml
		$(OFIND) -c $<i
		$(OFIND) -c $<

clean:
	rm *.cm*
	rm *.byte
	rm index.js
	rm scrabble.js
