let
  inherit (import ./constants.nix) name defaultGhcVersion;
  inherit (import (import ./default.nix).inputs.nixpkgs { }) haskell;
in
haskell.lib.buildStackProject {
  ghc = haskell.compiler.${defaultGhcVersion};
  inherit name;
}
