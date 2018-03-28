{ mkDerivation, ansi-terminal, ansi-wl-pprint, base
, base16-bytestring, bytestring, case-insensitive, containers
, contravariant, cryptonite, deepseq, directory, exceptions
, fetchgit, filepath, formatting, haskeline, http-client
, http-client-tls, insert-ordered-containers, lens-family-core
, memory, mtl, optparse-generic, parsers, prettyprinter
, prettyprinter-ansi-terminal, repline, scientific, stdenv, tasty
, tasty-hunit, text, transformers, trifecta, unordered-containers
, vector
}:
mkDerivation {
  pname = "dhall";
  version = "1.11.1";
  src = fetchgit {
    url = "https://github.com/dhall-lang/dhall-haskell.git";
    rev    = "4a085aa3d622886cf7dd96a1ad475ba914d5ab1f";
    sha256 = "0849rvv9m5rgxgvn60q2bwfr7m1syjkgxrrs4xafs10ymfdx0g9f";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base base16-bytestring bytestring case-insensitive
    containers contravariant cryptonite directory exceptions filepath
    formatting http-client http-client-tls insert-ordered-containers
    lens-family-core memory parsers prettyprinter
    prettyprinter-ansi-terminal scientific text transformers trifecta
    unordered-containers vector
  ];
  executableHaskellDepends = [
    ansi-terminal base haskeline mtl optparse-generic prettyprinter
    prettyprinter-ansi-terminal repline text trifecta
  ];
  testHaskellDepends = [
    base deepseq insert-ordered-containers prettyprinter tasty
    tasty-hunit text vector
  ];
  description = "A configuration language guaranteed to terminate";
  license = stdenv.lib.licenses.bsd3;
}
