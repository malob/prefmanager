{ nixpkgs ? import <nixpkgs> {}}:
nixpkgs.pkgs.haskell.packages.ghc865.callPackage ./prefmanager.nix { } 
