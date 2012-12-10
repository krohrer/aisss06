.PHONY: all byte native

OCAMLBUILD=ocamlbuild -use-ocamlfind

all: aisss06.native aisss06.byte

aisss06.native:
	$(OCAMLBUILD) $@

aisss06.byte:
	$(OCAMLBUILD) $@

clean:
	$(OCAMLBUILD) -clean
