{ mkDerivation, base, bytestring, dhall, dhall-json, directory
, filepath, Glob, megaparsec, nsis, optparse-applicative, split
, stdenv, system-filepath, temporary, text, trifecta, turtle
, universum, yaml
, cabal-install, nodejs, nix
}:
mkDerivation {
  pname = "daedalus-installer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring dhall dhall-json directory filepath Glob megaparsec
    nsis optparse-applicative split system-filepath temporary text
    trifecta turtle universum yaml
    cabal-install nodejs nix
  ];
  description = "Daedalus Installer Builder";
  license = stdenv.lib.licenses.mit;
}
