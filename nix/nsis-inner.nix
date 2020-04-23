{ stdenv, lib, fetchurl, scons, gcc, zlib, arch, buildGcc, buildPackages }:
let
  version = "3.03";
  sconsArgs = lib.concatStringsSep "\" \"" [
    "STRIP=0"
    "ZLIB_W32=${zlibJoin}"
    "TARGET_ARCH=${arch}"
    "PREFIX=$out"
    # TODO: note: building zip2exe fails due to zlib linking issues.
    "SKIPUTILS=NSIS Menu,zip2exe"
    "VERSION=${version}"
    "CC=${buildGcc}/bin/gcc"
    "CXX=${buildGcc}/bin/g++"
    "VERBOSE=1"
    "XGCC_W32_PREFIX=${if arch == "x86" then "i686" else "x86_64"}-pc-mingw32-"
    "APPEND_CPPPATH=${buildPackages.zlib.dev}/include"
    "APPEND_LIBPATH=${buildPackages.zlib}/lib"
    "PATH=$PATH"
  ];
  # the NSIS SCons script expects to find the `.dll` in /lib.
  # it also expects include and lib to be together.
  zlibJoin = buildPackages.buildEnv {
    name = "full-zlib";
    paths = [ zlib.dev zlib ];
    postBuild = ''
      ln -sv $out/bin/*.dll $out/lib
    '';
  };
in stdenv.mkDerivation {
  name = "nsis";
  src = fetchurl {
    url = "https://downloads.sourceforge.net/project/nsis/NSIS%203/${version}/nsis-${version}-src.tar.bz2";
    sha256 = "abae7f4488bc6de7a4dd760d5f0e7cd3aad7747d4d7cd85786697c8991695eaa";
  };

  nativeBuildInputs = [ scons ];

  buildPhase = ''
    scons makensis "${sconsArgs}"
  '';
  installPhase = ''
    scons install "${sconsArgs}"
  '';

  meta = with stdenv.lib; {
    descripition = "System to create Windows installers";
    homepage = "https://nsis.sourceforge.io/";
  };
}
