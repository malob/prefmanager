with import (import ./default.nix).inputs.nixpkgs {};
haskell.lib.buildStackProject {
  ghc = haskell.compiler.ghc8107;
  name = "prefmanager";
}
