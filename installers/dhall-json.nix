{ mkDerivation, aeson, aeson-pretty, base, bytestring, dhall
, fetchgit, optparse-generic, stdenv, text, trifecta
, unordered-containers, yaml
}:
mkDerivation {
  pname = "dhall-json";
  version = "1.0.12";
  src = fetchgit {
    url = "https://github.com/dhall-lang/dhall-json";
    sha256 = "0pvbpbg6475drvpakny12y3z2dv0vj6x4hlk853dgb84xbsd8i33";
    rev = "d6adaa265dcf8ab5899396b05d612b2d8092dca4";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring dhall text trifecta unordered-containers
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring dhall optparse-generic text yaml
  ];
  description = "Compile Dhall to JSON or YAML";
  license = stdenv.lib.licenses.bsd3;
}
