build:
	@dune build --profile release
	@rm -f minirust
	@cp _build/install/default/bin/minirust .

clean:
	@dune clean
	
fmt:
	@dune fmt
.PHONY: build