{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc843" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          monad-par mtl parsec cabal-install hoogle
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "k-gas-analyser";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
