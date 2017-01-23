OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)"

index.js: index.ml
	$(OCAMLBUILD) $@
	cp _build/$@ $@

.PHONY: clean

clean:
	rm index.js
	ocamlbuild -clean
