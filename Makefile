
setup:
	stack setup

build:
	stack build

install:
	stack build --copy-bins

repl:
	stack ghci

run: export TRASSH=$(HOME)/.local/bin/trahs-exe --server
run: install
	stack exec trahs-exe -- localhost:dir-server dir-client

.PHONY: release test loc clean
