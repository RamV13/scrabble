run:
	ocamlbuild -pkgs yojson grid.byte dictionary.byte game.byte ai.byte

top:
	ocamlbuild -pkgs yojson grid.cmo dictionary.cmo game.cmo && ocamlbuild -pkg yojson ai.inferred.mli

ai:
	ocamlbuild -pkg yojson ai.cmi && ocamlbuild -pkg yojson ai.cmo

clean:
	ocamlbuild -clean

test:
	ocamlbuild -pkgs oUnit,yojson ai_test.byte && ./ai_test.byte
