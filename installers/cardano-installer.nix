{ mkDerivation, aeson, base, bytestring, directory, filepath, Glob
, megaparsec, nsis, optparse-applicative, split, stdenv
, system-filepath, temporary, text, turtle, universum, yaml
}:
mkDerivation {
  pname = "cardano-installer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring directory filepath Glob megaparsec nsis
    optparse-applicative split system-filepath temporary text turtle
    universum yaml
  ];
  description = "Cardano Installer";
  license = stdenv.lib.licenses.mit;
}
