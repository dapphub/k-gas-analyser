{ nixpkgs ? import <nixpkgs> {}}:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          cabal-install utf8-string aeson parsec safe optparse-applicative
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "k-gas-analyser";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
