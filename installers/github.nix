{ mkDerivation, aeson, aeson-compat, base, base-compat
, base16-bytestring, binary, binary-orphans, byteable, bytestring
, containers, cryptohash, deepseq, deepseq-generics, exceptions
, file-embed, hashable, hspec, hspec-discover, http-client
, http-client-tls, http-link-header, http-types, iso8601-time, mtl
, network-uri, semigroups, stdenv, text, time, tls, transformers
, transformers-compat, unordered-containers, vector
, vector-instances
}:
mkDerivation {
  pname = "github";
  version = "0.18";
  sha256 = "31dc02d345e46b09bbc7ae7b898102630b2861b50814a13f6265c6929ad18c44";
  revision = "2";
  editedCabalFile = "1rywfb78acwh81mdnxb4q35n374k1wbxg0562biis0i0jjxfp211";
  libraryHaskellDepends = [
    aeson aeson-compat base base-compat base16-bytestring binary
    binary-orphans byteable bytestring containers cryptohash deepseq
    deepseq-generics exceptions hashable http-client http-client-tls
    http-link-header http-types iso8601-time mtl network-uri semigroups
    text time tls transformers transformers-compat unordered-containers
    vector vector-instances
  ];
  testHaskellDepends = [
    aeson-compat base base-compat bytestring file-embed hspec
    unordered-containers vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/phadej/github";
  description = "Access to the GitHub API, v3";
  license = stdenv.lib.licenses.bsd3;
}
