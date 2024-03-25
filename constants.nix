{
  name = "prefmanager";
  ghcVersions = map (v: "ghc${v}") [ "810" "90" "92" "94" "96" ];
  defaultGhcVersion = "ghc96";
}
