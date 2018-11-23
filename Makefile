all: build

default.nix: k-gas-analyser.cabal
	nix-shell --command 'cabal2nix . > default.nix'
.configured: default.nix shell.nix
	nix-shell --command 'cabal configure --enable-tests'
	touch .configured
build: .configured
	nix-shell --command 'cabal build'
