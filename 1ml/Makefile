#
# (c) 2014 Andreas Rossberg
#

NAME = 1ml
MODULES = \
  lib source prim syntax parser lexer \
  fomega types iL env erase trace sub elab \
  lambda compile \
  main
NOMLI = syntax iL main
PARSERS = parser
LEXERS = lexer
SAMPLES = prelude paper
TEXTS = README

MLS = $(MODULES:%=%.ml)
MLIS = $(filter-out $(NOMLI:%=%.mli), $(MODULES:%=%.mli))
MLYS = $(PARSERS:%=%.mly)
MLLS = $(LEXERS:%=%.mll)
CMOS = $(MODULES:%=%.cmo)
CMXS = $(MODULES:%=%.cmx)
IMLS = $(SAMPLES:%=%.1ml)
TXTS = $(TEXTS:%=%.txt)

WEB_MODULES = $(filter-out main, $(MODULES))
WEB_CMOS = $(WEB_MODULES:%=%.cmo)

all: $(NAME) web

$(NAME): $(CMXS) Makefile
	ocamlopt $(CMXS) -o $@

web: $(CMOS) ../docs/web.ml Makefile
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml-ppx $(WEB_CMOS) -linkpkg -o ../docs/web.byte ../docs/web.ml
	cd ../docs && js_of_ocaml web.byte -o js/web.js && rm web.byte

unopt: $(CMOS) Makefile
	ocamlc $(CMOS) -g -o $(NAME)

$(filter-out $(NOMLI:%=%.cmo), $(CMOS)): %.cmo: %.cmi
$(filter-out $(NOMLI:%=%.cmx), $(CMXS)): %.cmx: %.cmi

Makefile.depend: $(MLS) $(MLIS) Makefile
	ocamldep $^ >$@

-include Makefile.depend

zip: $(MLS) $(MLIS) $(MLYS) $(MLLS) Makefile $(IMLS) $(TXTS)
	mkdir tmp tmp/$(NAME)
	cp $^ tmp/$(NAME)
	rm -f $(NAME).zip
	(cd tmp; zip -r ../$(NAME).zip $(NAME))
	rm -r tmp

clean:
	rm -f *.cmi *.cmo *.cmx *.o *.output *.depend
	rm -f *.native *.byte $(NAME) $(NAME).opt $(NAME).zip
	rm -f *~
	rm -f ../docs/web.byte ../docs/js/web.js

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c -g $<

%.cmx: %.ml
	ocamlopt -c $<

%.ml: %.mly
	ocamlyacc -v $<

%.mli: %.mly
	ocamlyacc -v $<

%.ml: %.mll
	ocamllex $<

.PRECIOUS: %.ml %.mli
