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
