{
  name = "prefmanager";
  ghcVersions = map (v: "ghc${v}") [ "8107" "902" "928" "945" "962" ];
  defaultGhcVersion = "ghc928";
}
