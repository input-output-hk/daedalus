{ mkDerivation, aeson, base, bytestring, containers, dhall
, dhall-json, directory, filepath, github, Glob, lens-aeson
, megaparsec, microlens, network-uri, nsis, optional-args
, optparse-applicative, optparse-generic, split, stdenv
, system-filepath, temporary, text, turtle, universum, wreq, yaml
, zip-archive
}:
mkDerivation {
  pname = "daedalus-installer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers dhall dhall-json directory
    filepath github Glob lens-aeson megaparsec microlens network-uri
    nsis optional-args optparse-applicative optparse-generic split
    system-filepath temporary text turtle universum wreq yaml
    zip-archive
  ];
  description = "Daedalus Installer Builder";
  license = stdenv.lib.licenses.mit;
}
