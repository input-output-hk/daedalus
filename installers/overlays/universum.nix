{ mkDerivation, base, bytestring, containers, deepseq, doctest
, gauge, ghc-prim, Glob, hashable, hedgehog, microlens
, microlens-mtl, mtl, safe-exceptions, stdenv, stm, tasty
, tasty-hedgehog, text, transformers, unordered-containers
, utf8-string, vector
}:
mkDerivation {
  pname = "universum";
  version = "1.5.0";
  sha256 = "53d29c4de630320c4364d37ea26a150c40e8df7faf81f69bb94372314f883f9f";
  libraryHaskellDepends = [
    base bytestring containers deepseq ghc-prim hashable microlens
    microlens-mtl mtl safe-exceptions stm text transformers
    unordered-containers utf8-string vector
  ];
  testHaskellDepends = [
    base bytestring doctest Glob hedgehog tasty tasty-hedgehog text
    utf8-string
  ];
  benchmarkHaskellDepends = [
    base containers gauge unordered-containers
  ];
  homepage = "https://github.com/serokell/universum";
  description = "Custom prelude used in Serokell";
  license = stdenv.lib.licenses.mit;
}
