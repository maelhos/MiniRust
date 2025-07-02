build:
	@dune build
	@rm -f MiniRust
	@cp _build/install/default/bin/MiniRust .

.PHONY: build