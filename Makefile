default: format
	dune build

lint:
	make clean
	dune build @lint
	dune build @fmt

format:
	dune build @fmt --auto-promote

release: format
	dune build --profile release

clean:
	dune clean

test: default
	dune runtest

install: release
	dune install

uninstall: release
	dune uninstall

.PHONY: default format release clean test test-validate install uninstall
