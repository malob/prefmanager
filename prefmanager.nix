{ mkDerivation, async, base, containers, diffmap, hpack, hxt
, optparse-applicative, plist, pretty-show, process, stdenv
}:
mkDerivation {
  pname = "prefmanager";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base containers diffmap hxt optparse-applicative plist
    pretty-show process
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    async base containers diffmap hxt optparse-applicative plist
    pretty-show process
  ];
  testHaskellDepends = [
    async base containers diffmap hxt optparse-applicative plist
    pretty-show process
  ];
  prePatch = "hpack";
  homepage = "https://github.com/malob/prefmanager#readme";
  description = "A CLI utility for managing macOS preferences";
  license = stdenv.lib.licenses.bsd3;
}
