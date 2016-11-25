
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
	stack exec trahs-exe -- localhost:"$(HOME)/work/trahs/remote" "$(HOME)/work/trahs"

.PHONY: release test loc clean
