{ mkDerivation, base, directory, filepath, Glob, megaparsec, nsis
, stdenv, temporary, text, turtle, aeson, yaml, universum
}:
mkDerivation {
  pname = "cardano-installer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base directory filepath Glob megaparsec nsis temporary text turtle universum yaml
  ];
  description = "Cardano Installer";
  license = stdenv.lib.licenses.mit;
}
