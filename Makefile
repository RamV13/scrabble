run:
	ocamlbuild -pkgs yojson,str grid.byte dictionary.byte game.byte ai.byte

top:
	ocamlbuild -pkgs yojson,str grid.cmo dictionary.cmo game.cmo && ocamlbuild -pkg yojson ai.inferred.mli && cp _build/ai.inferred.mli ai.mli

ai:
	ocamlbuild -use-ocamlfind -pkg yojson,str ai.cmi && ocamlbuild -use-ocamlfind -pkg yojson ai.cmo

game:
	ocamlbuild -use-ocamlfind -pkg yojson,str game.cmi && ocamlbuild -use-ocamlfind -pkg yojson game.cmo

clean:
	ocamlbuild -clean

aitest:
	ocamlbuild -pkgs oUnit,yojson,str ai_test.byte && ./ai_test.byte

gametest:
	ocamlbuild -pkgs oUnit,yojson game_test.byte && ./game_test.byte
