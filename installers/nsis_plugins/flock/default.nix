{ stdenv }:

stdenv.mkDerivation {
  name = "nsis-flock";
  src = ./src;
  installPhase = ''
    ls -ltrh
    mkdir -p $out/lib
    cp -v flock.dll $out/lib/
    $OBJDUMP -d libflock_dll.a
  '';
}
