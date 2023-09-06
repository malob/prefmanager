{
  name = "prefmanager";
  # GHC 9.6.2 disabled since `fgl` a dependency of `async-pool` fails to build.
  ghcVersions = map (v: "ghc${v}") [ "8107" "902" "928" "946" /* "962" */ ];
  defaultGhcVersion = "ghc946";
}
