{
  description = "Tool for working with macOS defaults.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    devshell.url = "github:numtide/devshell/";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    flake-utils.url = "github:numtide/flake-utils";
    plist-source = { url = "github:malob/plist/monadfail"; flake = false; };
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    devshell.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, devshell, nixpkgs, flake-utils, plist-source, ... }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" "aarch64-darwin" ] (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      compiler =  pkgs.haskell.packages.ghc902;
      hlib = pkgs.haskell.lib;
      plist = hlib.markUnbroken (hlib.overrideSrc compiler.plist { src = plist-source; });
      prefmanager = compiler.callCabal2nix "prefmanager" ./. { inherit plist; };
      mkShell = devshell.legacyPackages.${system}.mkShell;
    in rec {
      # Built by `nix build .`
      packages.default = prefmanager;
      packages.prefmanager = prefmanager;

      # # This is used by `nix develop .`
      devShell = mkShell {
        name = "prefmanager";
        packages = [
          compiler.haskell-language-server
          compiler.implicit-hie
          compiler.weeder
          pkgs.stack
          pkgs.hlint
        ];
        commands = [
          {
            help = "Regenerate hie.yaml (run from project root)";
            name = "hie";
            category = "project";
            command = "gen-hie > hie.yaml";
          }
        ];
      };
    }
  );
}
