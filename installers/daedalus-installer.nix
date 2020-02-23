{ mkDerivation, aeson, base, bytestring, containers, directory
, filepath, foldl, github, Glob, hspec, lens-aeson, managed
, megaparsec, microlens, network-uri, nsis, optional-args
, optparse-applicative, optparse-generic, split, stdenv
, system-filepath, temporary, text, turtle, universum, raw-strings-qq
, unordered-containers, wreq, yaml, zip-archive
}:
mkDerivation {
  pname = "daedalus-installer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  libraryHaskellDepends = [
    aeson base bytestring containers directory github Glob lens-aeson
    megaparsec microlens network-uri nsis optional-args raw-strings-qq
    system-filepath text turtle universum unordered-containers wreq
    yaml zip-archive
  ];
  executableHaskellDepends = [
    base bytestring containers directory filepath foldl optional-args
    optparse-applicative optparse-generic raw-strings-qq split
    system-filepath temporary text turtle universum yaml
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath foldl github
    hspec lens-aeson managed megaparsec optional-args
    optparse-applicative optparse-generic raw-strings-qq split
    system-filepath temporary text turtle universum yaml
  ];
  description = "Daedalus Installer Builder";
  license = stdenv.lib.licenses.asl20;
}
