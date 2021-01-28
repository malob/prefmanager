{ mkDerivation, ansi-terminal, async, base-noprelude, containers
, hpack, hxt, optparse-applicative, patience, plist, prettyprinter
, prettyprinter-ansi-terminal, process, relude, stdenv, text
}:
mkDerivation {
  pname = "prefmanager";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal async base-noprelude containers hxt patience plist
    prettyprinter prettyprinter-ansi-terminal process relude text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base-noprelude optparse-applicative ];
  testHaskellDepends = [ base-noprelude ];
  prePatch = "hpack";
  homepage = "https://github.com/malob/prefmanager#readme";
  description = "A CLI utility for managing macOS preferences";
  license = stdenv.lib.licenses.bsd3;
}
