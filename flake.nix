{
  description = "Tool for working with macOS defaults.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, haskellNix, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-darwin" ] (system: let
      pkgs = import nixpkgs { inherit system; overlays = [ haskellNix.overlay ]; };
      name = "prefmanager";
      compiler = "ghc884";
      project = pkgs.haskell-nix.project' {
        inherit name;
        src = self;
        compiler-nix-name = compiler;
      };
      components = project.hsPkgs.${name}.components;
    in rec {
      # Built by `nix build .`
      defaultPackage = components.exes.${name};
      packages.${name} = defaultPackage;

      # Run `prefmanager` with `nix run .`
      defaultApp = { type = "app"; program = components.exes.${name}.exePath; };
      apps.${name} = defaultApp;

      # This is used by `nix develop .`
      devShell = project.shellFor {
        buildInputs = [
          pkgs.haskell.packages.${compiler}.haskell-language-server
          pkgs.haskell.packages.${compiler}.implicit-hie
          pkgs.stack
        ];
      };
    }
  );
}
