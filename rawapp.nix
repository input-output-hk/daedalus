{ fetchurl, master_config, runCommand, xar, cpio }:

let
  darwinPackage = fetchurl {
    url = "http://s3.eu-central-1.amazonaws.com/daedalus-travis/Daedalus-installer-1.0.${master_config.daedalus_build_number}.pkg";
    sha256 = master_config.daedalus_hash;
  };
in runCommand "daedalus-app" { buildInputs = [ xar cpio ]; } ''
  xar -xf ${darwinPackage}
  cat $NIX_BUILD_TOP/temp.pkg/Payload | gunzip | cpio -i
  cp -vir Daedalus.app/Contents/Resources/app/ $out
''
