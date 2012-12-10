.PHONY: all byte native

OCAMLBUILD=ocamlbuild -use-ocamlfind
PROG=aisss06

all: native byte

native:
	$(OCAMLBUILD) $(PROG).native

byte:
	$(OCAMLBUILD) $(PROG).byte

clean:
	$(OCAMLBUILD) -clean
