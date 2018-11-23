all: build

build: default.nix
	nix-build
