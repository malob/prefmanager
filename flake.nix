{
  description = "Tool for working with macOS defaults.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    devshell.url = "github:numtide/devshell/";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    flake-utils.url = "github:numtide/flake-utils";
    plist-source = { url = "github:malob/plist/monadfail"; flake = false; };
    devshell.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, devshell, nixpkgs, flake-utils, plist-source, ... }:
    let
      inherit (import ./constants.nix) name ghcVersions defaultGhcVersion;
    in
    {
      overlays.${name} = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = final.lib.composeExtensions
            prev.haskell.packageOverrides
            (hfinal: hprev: {
              plist = final.haskell.lib.markUnbroken (final.haskell.lib.overrideSrc hprev.plist {
                src = plist-source;
              });
              ${name} = hfinal.callCabal2nix name ./. { };
            });
        };

        ${name} = final.haskell.packages.${defaultGhcVersion}.${name};
      };
    } // flake-utils.lib.eachSystem [ "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.${name} ]; };
      in
      {
        packages = {
          default = pkgs.${name};
          ${name} = pkgs.${name};

          # Create package for all `ghcVersions`.
        } // builtins.listToAttrs (map
          (v: { name = "${name}-${v}"; value = pkgs.haskell.packages.${v}.${name}; })
          ghcVersions
        );

        devShells.default = devshell.legacyPackages.${system}.mkShell {
          inherit name;

          packages = builtins.attrValues {
            inherit (pkgs.haskell.packages.${defaultGhcVersion})
              haskell-language-server
              hlint
              implicit-hie
              weeder
              ;

            inherit (pkgs) stack;
          };

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
