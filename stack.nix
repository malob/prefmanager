{ ghc }:
with import (import ./default.nix).inputs.nixpkgs {};
haskell.lib.buildStackProject {
  inherit ghc;
  name = "prefmanager";
}
