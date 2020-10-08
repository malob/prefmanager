let
  pkgs = import (import ./nix/sources.nix).nixpkgs {};
  hsPkgs = import ./default.nix {};
  compiler-name = hsPkgs.prefmanager.project.pkg-set.config.compiler.nix-name;
in hsPkgs.shellFor {
  buildInputs = [
    pkgs.haskell.packages.${compiler-name}.haskell-language-server
    pkgs.stack
    pkgs.niv
  ];
}
