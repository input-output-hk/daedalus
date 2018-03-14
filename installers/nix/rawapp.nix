{ fetchurl, master_config, runCommand, xar, cpio }:

let
  darwinPackage = fetchurl {
    url = master_config.daedalus_darwin_url;
    sha256 = master_config.daedalus_hash;
  };
in runCommand "daedalus-app" { buildInputs = [ xar cpio ]; } ''
  xar -xf ${darwinPackage}
  cat $NIX_BUILD_TOP/temp.pkg/Payload | gunzip | cpio -i
  cp -vir Daedalus.app/Contents/Resources/app/ $out
''
