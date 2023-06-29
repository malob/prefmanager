{
  name = "prefmanager";
  # GHC 9.4.5 and 9.6.2 are disabled since `fgl` a dependency of `async-pool` fails to build.
  ghcVersions = map (v: "ghc${v}") [ "8107" "902" "928" /* "945" "962" */ ];
  defaultGhcVersion = "ghc928";
}
