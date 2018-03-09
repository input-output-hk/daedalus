{ runCommandCC, rawapp, electron, nukeReferences, xlibs, libudev, nss, cardanoPkgs, pkgs, runCommand, writeScript, cluster, openssl, configFiles }:

let
  daedalus = writeScript "daedalus" ''
    #!/usr/bin/env bash

    set -x
    set -e
    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
    export PATH=$DAEDALUS_DIR/installation/bin:$PATH

    mkdir -p "''${DAEDALUS_DIR}/${cluster}/"{logs/pub,Secrets}
    cd "''${DAEDALUS_DIR}/${cluster}/"

    if [ ! -d tls ]; then
      mkdir -p tls/{server,ca}
      openssl req -x509 -newkey rsa:2048 -keyout tls/server/server.key -out tls/server/server.crt -days 3650 -nodes -subj "/CN=localhost"
      cp tls/server/server.crt tls/ca/ca.crt
    fi

    configFiles=$DAEDALUS_DIR/etc

    cardano-launcher --node cardano-node \
      --node-log-path logs/cardano-node.log \
      --db-path LDB/ \
      --wallet frontend \
      --launcher-logs-prefix logs/pub/ \
      --node-timeout 30 \
      --updater /not-supported \
      --configuration-file $configFiles/configuration.yaml \
      --configuration-key mainnet_wallet_macos64 \
      -n --report-server -n http://report-server.cardano-mainnet.iohk.io:8080 \
      -n --log-config -n $configFiles/daedalus.yaml \
      -n --keyfile -n Secrets/secret.key \
      -n --db-path -n DB/ \
      -n --wallet-db-path -n Wallet/ \
      -n --no-ntp \
      -n --tlscert -n tls/server/server.crt \
      -n --tlskey -n tls/server/server.key \
      -n --tlsca -n tls/ca/ca.crt \
      -n --configuration-file -n $configFiles/configuration.yaml \
      -n --configuration-key -n mainnet_wallet_macos64 \
      -n --wallet-address -n 127.0.0.1:8090 \
      -n --topology -n $configFiles/topology.yaml
  '';
  frontend = writeScript "frontend" ''
    #!/usr/bin/env bash

    set -x
    set -e
    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    exec $DAEDALUS_DIR/installation/bin/electron $DAEDALUS_DIR/installation/frontend
  '';
  daedalusDir = runCommandCC "daedalus-tarball" {
    buildInputs = [ nukeReferences ];
    allowedReferences = [ "out" ];
  }
  ''
    mkdir -p $out/{bin,lib,etc}

    cp ${electron}/lib/electron/.electron-wrapped $out/bin/electron
    cp -L ${libudev.lib}/lib/libudev.so.1 $out/lib/
    cp -L ${nss}/lib/{libsoftokn3.so,libfreeblpriv3.so} $out/lib/

    for file in icudtl.dat snapshot_blob.bin natives_blob.bin locales content_resources_200_percent.pak pdf_viewer_resources.pak blink_image_resources_200_percent.pak views_resources_200_percent.pak resources content_shell.pak ui_resources_200_percent.pak; do
      cp -r ${electron}/lib/electron/$file $out/bin/
    done
    cp -L ${xlibs.libXScrnSaver}/lib/libXss.so.1 $out/lib/

    cp ${cardanoPkgs.cardano-sl-tools}/bin/cardano-launcher $out/bin/
    cp ${cardanoPkgs.cardano-sl-wallet}/bin/cardano-node $out/bin/
    cp ${daedalus} $out/bin/daedalus
    cp ${frontend} $out/bin/frontend
    cp -r ${rawapp} $out/frontend
    cp ${openssl}/bin/openssl $out/bin/
    cp ${configFiles}/* $out/etc/

    # mostly copied from nixpkgs/nixos/modules/system/boot/stage-1.nix

    # Copy all of the needed libraries
    find $out/bin $out/lib -type f | while read BIN; do
      echo "Copying libs for executable $BIN"
      LDD="$(ldd $BIN)" || continue
      LIBS="$(echo "$LDD" | awk '{print $3}' | sed '/^$/d')"
      for LIB in $LIBS; do
        TGT="$out/lib/$(basename $LIB)"
        if [ ! -f "$TGT" ]; then
          SRC="$(readlink -e $LIB)"
          cp -pdv "$SRC" "$TGT"
        fi
      done
    done

    chmod -R u+w $out

    find $out/bin $out/lib -type f | while read i; do
      if ! test -L $i; then
        nuke-refs -e $out $i
      fi
    done
  '';
  tarball2 = runCommand "daedalusTarball.tar.xz" {} ''
    cd ${daedalusDir}
    tar -cJf $out *
  '';
  tarball = pkgs.callPackage (pkgs.path + "/nixos/lib/make-system-tarball.nix") {
    fileName = "daedalusTarball";
    contents = [];
    storeContents = [
      {
        object = daedalusDir;
        symlink = "/daedalusDir";
      }
    ];
  };
in tarball2
