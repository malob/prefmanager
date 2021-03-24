with import (import ./default.nix).inputs.nixpkgs {};
haskell.lib.buildStackProject {
  ghc = haskell.compiler.ghc8104;
  name = "prefmanager";
}
