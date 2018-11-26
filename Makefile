NIX_TARGET=result/bin/k-gas-analyser

all: build

build: default.nix
	nix-build

test: test-help

test-help: $(NIX_TARGET)
	./$(NIX_TARGET) --help
