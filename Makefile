build:
	@dune build
	@rm -f minirust
	@cp _build/install/default/bin/minirust .

.PHONY: build