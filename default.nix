{
  nixpkgs ? (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "2c2ab68672a34e7c226ff0c5ecf4d2e063ac5705";
    sha256 = "00rsmyhq50c44ayiwcaz8ck2043lrywyj9mj4cad8aw8f3l6yq6x";
  },
  compiler ? "ghc822"
}:
let
  pkgs = import nixpkgs { config = {}; };
in
  pkgs.haskell.packages.${compiler}.callPackage ./analyser.nix { }
