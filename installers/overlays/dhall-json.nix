{ mkDerivation, aeson, aeson-pretty, ansi-terminal, base
, bytestring, containers, dhall, exceptions, filepath, libyaml
, optparse-applicative, prettyprinter, prettyprinter-ansi-terminal
, scientific, stdenv, tasty, tasty-hunit, text
, unordered-containers, vector, yaml
}:
mkDerivation {
  pname = "dhall-json";
  version = "1.4.1";
  sha256 = "f70ef4d4c34624e761e339977a01d7cac0b0bbef228231d25f46729ddfdd2df2";
  revision = "1";
  editedCabalFile = "0vwr27ikw0y39za9jc91g3xbd7vb745zkkni0x3k73944w0w47n3";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring containers dhall exceptions
    filepath libyaml optparse-applicative prettyprinter scientific text
    unordered-containers vector yaml
  ];
  executableHaskellDepends = [
    aeson aeson-pretty ansi-terminal base bytestring dhall exceptions
    optparse-applicative prettyprinter prettyprinter-ansi-terminal text
  ];
  testHaskellDepends = [
    aeson base bytestring dhall tasty tasty-hunit text
  ];
  description = "Convert between Dhall and JSON or YAML";
  license = stdenv.lib.licenses.bsd3;
}
