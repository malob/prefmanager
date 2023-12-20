{
  name = "prefmanager";
  # GHC 9.6.2 disabled since `fgl` a dependency of `async-pool` fails to build.
  ghcVersions = map (v: "ghc${v}") [ "810" "90" "92" "94" "96" ];
  defaultGhcVersion = "ghc94";
}
