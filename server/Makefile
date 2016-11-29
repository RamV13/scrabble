
OBJS=dictionary.cmo grid.cmo game.cmo httpServer.cmo
NAME=server
OFIND=ocamlfind ocamlc -thread -package cohttp.lwt,cohttp.async,lwt.ppx,sqlite3,yojson

$(NAME).byte: $(OBJS)
		$(OFIND) -linkpkg -o $@ $(OBJS) $(NAME).ml

%.cmo: %.ml
		$(OFIND) -c $<i
		$(OFIND) -c $<

clean:
		rm *.cm*
		rm *.byte
