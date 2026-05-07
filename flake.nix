{
  description = "Tool for working with macOS defaults.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    haskell-flake.url = "github:srid/haskell-flake";

    # Fork of `plist` that supports MonadFail; upstream is unmaintained.
    plist-source = {
      url = "github:malob/plist/monadfail";
      flake = false;
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-darwin" "aarch64-darwin" ];

      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { pkgs, ... }:
        let
          mkProject = ghc: {
            basePackages = pkgs.haskell.packages.${ghc};
            packages.plist.source = inputs.plist-source;
            settings.plist.broken = false;
          };
        in
        {
          # Default project — exposes `nix build .#prefmanager`, `nix run .`, dev shell.
          haskellProjects.default = mkProject "ghc910";

          # Extra GHCs in the matrix — exposed as `nix build .#ghc912-prefmanager`, etc.
          # Adding a new GHC = one new line here plus a CI matrix entry.
          haskellProjects.ghc912 = mkProject "ghc912" // {
            autoWire = [ "packages" ];
          };
          haskellProjects.ghc914 = pkgs.lib.recursiveUpdate (mkProject "ghc914") {
            autoWire = [ "packages" ];
            # relude-1.2.2.2's doctests fail under GHC 9.14 (output format
            # drift; the library itself builds clean). Skip the test phase
            # rather than wait on a relude release.
            settings.relude.check = false;
          };
        };
    };
}
