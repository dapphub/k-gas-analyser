# k-gas-analyser

### installation

requires [nix](https://nixos.org/nix/)

```sh
$ git clone git@github.com:dapphub/k-gas-analyser.git && cd k-gas-analyser
$ nix-shell
[nix-shell]$ ghc Analyser
```

### example usage

```sh
$ ./Analyser --input examples/Token_transfer_pass.kast.json
```
