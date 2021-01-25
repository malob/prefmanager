{
  description = "Tool for working with macOS defaults.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    devshell.url = "github:numtide/devshell/";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    flake-utils.url = "github:numtide/flake-utils";
    plist-source = { url = "github:malob/plist/monadfail"; flake = false; };
  };

  outputs = { self, devshell, nixpkgs, flake-utils, plist-source, ... }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" ] (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      compiler =  pkgs.haskell.packages.ghc8103;
      hlib = pkgs.haskell.lib;
      plist = hlib.markUnbroken (hlib.overrideSrc compiler.plist { src = plist-source; });
      prefmanager = compiler.callPackage ./prefmanager.nix { inherit plist; };
      mkShell = devshell.legacyPackages.${system}.mkShell;
    in rec {
      # Built by `nix build .`
      defaultPackage = prefmanager;
      packages.prefmanager = defaultPackage;

      # Run `prefmanager` with `nix run .`
      defaultApp = { type = "app"; program = "${prefmanager}/bin/prefmanager"; };
      apps.prefmanager = defaultApp;

      # # This is used by `nix develop .`
      devShell = mkShell {
        name = "prefmanager";
        packages = [
          compiler.haskell-language-server
          compiler.implicit-hie
          pkgs.cabal2nix
          pkgs.stack
        ];
        commands = [
          {
            help = "Regenerate hie.yaml (run from project root)";
            name = "hie";
            category = "project";
            command = "gen-hie > hie.yaml";
          }
          {
            help = "Update prefmanager.nix (run from project root)";
            name = "2nix";
            category = "project";
            command = "cabal2nix --hpack . > prefmanager.nix";
          }
        ];
      };
    }
  );
}
