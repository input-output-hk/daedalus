{ mkDerivation, base, directory, fetchFromGitHub, process, stdenv
, transformers, uniplate
}:
mkDerivation {
  pname = "nsis";
  version = "0.3.2";
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "haskell-nsis";
    rev = "020e61eced93eaa6ab86ac603617e93aa6bf5af0";
    sha256 = "0l0naknnyyrmkrn41mn7ggnjdagld0isdji98khn2iasbcllyip5";
  };
  libraryHaskellDepends = [ base transformers uniplate ];
  testHaskellDepends = [
    base directory process transformers uniplate
  ];
  homepage = "https://github.com/ndmitchell/nsis#readme";
  description = "DSL for producing Windows Installer using NSIS";
  license = stdenv.lib.licenses.bsd3;
}
