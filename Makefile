OBJS=grid.cmo dictionary.cmo
NAME=game
OFIND=ocamlfind ocamlc -package yojson

$(NAME).byte: $(OBJS)
		$(OFIND) -linkpkg -o $@ $(OBJS) $(NAME).ml

%.cmo: %.ml
		$(OFIND) -c $<i
		$(OFIND) -c $<

clean:
		rm *.cm*
		rm *.byte
run:
	ocamlbuild -pkgs yojson,str grid.byte dictionary.byte game.byte ai.byte

top:
	ocamlbuild -pkgs yojson,str grid.cmo dictionary.cmo game.cmo && ocamlbuild -pkg yojson ai.inferred.mli && cp _build/ai.inferred.mli ai.mli

ai:
	ocamlbuild -use-ocamlfind -pkg yojson,str ai.cmi && ocamlbuild -use-ocamlfind -pkg yojson ai.cmo

clean:
	ocamlbuild -clean

test:
	ocamlbuild -pkgs oUnit,yojson,str ai_test.byte && ./ai_test.byte
