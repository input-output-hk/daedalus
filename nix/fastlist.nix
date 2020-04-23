{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation {
  name = "fastlist";
  src = fetchFromGitHub {
    owner = "MarkTiedemann";
    repo = "fastlist";
    rev = "65a9eaefa802fc4d3d3095f01a0321fd073a9098";
    sha256 = "0mw6x54n7bmi0fqw8drahcfk6yv232mymp476gy75h1sclk90fsa";
  };
  buildCommand = ''
    unpackPhase
    cd $sourceRoot
    mkdir -p $out/bin/
    $CC fastlist.cpp -o $out/bin/fastlist.exe
  '';
}
