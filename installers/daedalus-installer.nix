{ mkDerivation, aeson, base, bytestring, containers, dhall
, dhall-json, directory, filepath, foldl, github, Glob, hspec
, lens-aeson, managed, megaparsec, microlens, network-uri, nsis
, optional-args, optparse-applicative, optparse-generic, split
, stdenv, system-filepath, temporary, text, turtle, universum, wreq
, yaml, zip-archive
}:
mkDerivation {
  pname = "daedalus-installer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers dhall dhall-json directory github
    Glob lens-aeson megaparsec microlens network-uri nsis optional-args
    system-filepath text turtle universum wreq yaml zip-archive
  ];
  executableHaskellDepends = [
    aeson base bytestring containers dhall dhall-json directory
    filepath foldl megaparsec optional-args optparse-applicative
    optparse-generic split system-filepath temporary text turtle
    universum yaml
  ];
  testHaskellDepends = [
    aeson base bytestring containers dhall dhall-json directory
    filepath foldl hspec lens-aeson managed megaparsec optional-args
    optparse-applicative optparse-generic split system-filepath
    temporary text turtle universum yaml
  ];
  description = "Daedalus Installer Builder";
  license = stdenv.lib.licenses.mit;
}
