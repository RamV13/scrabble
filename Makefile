run:
	ocamlbuild -pkgs yojson grid.byte dictionary.byte game.byte ai.byte

top:
	ocamlbuild -pkgs yojson grid.cmo dictionary.cmo game.cmo

clean:
	ocamlbuild -clean

test:
	ocamlbuild -pkg oUnit ai_test.byte && ./ai_test.byte
