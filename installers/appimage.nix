{ lib, stdenv
, fetchurl, writeScriptBin, runCommand
, patchelf, pax-utils, dpkg
, makeAppImage
, frontend
, daedalus
, electronBinaries
, version
}:

let
  update-runner = writeScriptBin "update-runner" ''
    #!/bin/sh

    set -eu

    if [ ! -e "$1" ]; then
      echo "update file not found"
      exit -1
    fi

    if [ -z "$APPIMAGE" ]; then
      echo "APPIMAGE variable is not set"
      exit -2
    fi

    mv --backup "$1" "$APPIMAGE"
  '';

  daedalus-frontend = writeScriptBin "daedalus-frontend" ''
    #!/bin/sh
    top=$(cd $(dirname $0)/../..; pwd)
    # Don't share libs with cardano -- take them from the host OS.
    export LD_LIBRARY_PATH=$top/electron:/usr/lib:/usr/lib32
    exec $top/electron/electron $top/usr/share/daedalus/app/main/
  '';


  # Precompiled library dependencies of electron
  moreElectronLibs = let
    gconfPackage = fetchurl {
      url = http://archive.ubuntu.com/ubuntu/pool/universe/g/gconf/libgconf-2-4_3.2.6-4ubuntu1_amd64.deb;
      sha256 = "0z0y3131c58zxkq4q2pabfm1h4ihjif6v4jchjyf35xn3rw2ry82";
    };
  in runCommand "libgconf-2" {} ''
    ${dpkg}/bin/dpkg-deb -x ${gconfPackage} .
    mv ./usr/lib/x86_64-linux-gnu $out
  '';

  appImage = makeAppImage {
    inherit (daedalus) name;
    inherit version;
    passthru = { inherit (daedalus) network; };

    buildInputs = [ pax-utils patchelf ];
    buildCommand = ''
      mkdir -p $out

      # add daedalus, cardano, configs
      cp -R --dereference ${daedalus} $out/usr
      chmod -R +w $out/usr
      rm -rf $out/usr/libexec/daedalus-frontend $out/usr/libexec/update-runner
      ln -sf daedalus $out/usr/bin/daedalus-testnet
      ln -sf daedalus $out/usr/bin/daedalus-staging

      # add the frontend
      cp -R ${frontend}/share/daedalus $out/usr/share/daedalus/app
      chmod -R +w $out/usr/share/daedalus/app

      # Add package.json so that electron knows the app name
      cp ${frontend.packageJSON} $out/usr/share/daedalus/app/main/package.json

      # add electron binaries
      cp -R ${electronBinaries} $out/electron
      chmod +w $out/electron
      cp ${moreElectronLibs}/* $out/electron

      # Find all dynamic libraries needed for cardano and copy.
      # It uses lddtree to recursively find all shared object dependencies.
      # It also copies the other libraries in the same directory as each
      # dependency, so that things like NSS modules are picked up.
      find_libs() {
          for bin in $out/usr/libexec/*; do
              lddtree -l $bin | grep -v $bin | grep -v ld-linux | grep '^/nix/store' | while read lib; do
                  dirname $lib
              done
          done | sort | uniq
      }
      mkdir -p $out/usr/lib
      for lib in $(find_libs); do
          echo "Bundling $lib"
          cp --no-preserve=mode -d $lib/*.so* $out/usr/lib
      done

      chmod -R +w $out/usr/bin $out/usr/libexec $out/usr/lib $out/electron

      # remove /nix/store RPATHs and nix interpreter
      for bin in $out/usr/libexec/*; do
        patchelf --remove-rpath --no-default-lib --set-interpreter /lib64/ld-linux-x86-64.so.2 $bin
      done
      for lib in $out/usr/lib/*.so*; do
        patchelf --remove-rpath --no-default-lib $lib || true
      done

      cp ${daedalus-frontend}/bin/daedalus-frontend ${update-runner}/bin/update-runner $out/usr/libexec

      cd $out
      ln -s usr/share/applications/*.desktop $out/Daedalus-Cardano-SL.desktop
      ln -s usr/share/icons/*/*/apps/*.png $out/Daedalus-Cardano-SL.png
    '';
  };

in
  appImage
