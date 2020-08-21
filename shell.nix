{ pkgs ? import (import ./nix/sources.nix).nixpkgs {} }:

with pkgs;
mkShell {
  buildInputs = [
    haskell.packages.ghc865.haskell-language-server
    cabal2nix
    stack
    niv
  ];
}
