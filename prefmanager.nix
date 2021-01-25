{ mkDerivation, async, base, containers, hpack, hxt
, optparse-applicative, patience, plist, prettyprinter
, prettyprinter-ansi-terminal, process, stdenv, text
}:
mkDerivation {
  pname = "prefmanager";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base containers hxt optparse-applicative patience plist
    prettyprinter prettyprinter-ansi-terminal process text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    async base containers hxt optparse-applicative patience plist
    prettyprinter prettyprinter-ansi-terminal process text
  ];
  testHaskellDepends = [
    async base containers hxt optparse-applicative patience plist
    prettyprinter prettyprinter-ansi-terminal process text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/malob/prefmanager#readme";
  description = "A CLI utility for managing macOS preferences";
  license = stdenv.lib.licenses.bsd3;
}
