
OBJS=dictionary.cmo grid.cmo game.cmo ai.cmo httpServer.cmo
NAME=server
OFIND=ocamlfind ocamlc -thread -package cohttp.lwt,cohttp.async,lwt.ppx,str,yojson

$(NAME).byte: $(OBJS)
		$(OFIND) -linkpkg -o $@ $(OBJS) $(NAME).ml

%.cmo: %.ml
		$(OFIND) -c $<i
		$(OFIND) -c $<

clean:
		ocamlbuild -clean
		rm *.cm*
		rm *.byte

run:
	ocamlbuild -pkgs yojson,str grid.byte dictionary.byte game.byte ai.byte

top:
	ocamlbuild -pkgs yojson,str grid.cmo dictionary.cmo game.cmo && ocamlbuild -pkg yojson ai.inferred.mli && cp _build/ai.inferred.mli ai.mli

ai:
	ocamlbuild -use-ocamlfind -pkg yojson,str ai.cmi && ocamlbuild -use-ocamlfind -pkg yojson ai.cmo

game:
	ocamlbuild -use-ocamlfind -pkg yojson,str game.cmi && ocamlbuild -use-ocamlfind -pkg yojson game.cmo

test:
	ocamlbuild -pkgs oUnit,yojson dict_test.byte && ./dict_test.byte
	ocamlbuild -pkgs oUnit,yojson grid_test.byte && ./grid_test.byte
	ocamlbuild -pkgs oUnit,yojson game_test.byte && ./game_test.byte
	ocamlbuild -pkgs oUnit,yojson,str ai_test.byte && ./ai_test.byte

aitest:
	ocamlbuild -pkgs oUnit,yojson,str ai_test.byte && ./ai_test.byte

gametest:
	ocamlbuild -pkgs oUnit,yojson game_test.byte && ./game_test.byte

gridtest:
	ocamlbuild -pkgs oUnit,yojson grid_test.byte && ./grid_test.byte

dicttest:
	ocamlbuild -pkgs oUnit,yojson dict_test.byte && ./dict_test.byte
