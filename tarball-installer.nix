{ tarball, writeScript, writeScriptBin, stdenv, lib, coreutils, gnutar, xz, pv, gnused, patchelf,
makeDesktopItem, nix-bundle }:

let
  iconPath = ./installers/icons/1024x1024.png;
  desktopItem = makeDesktopItem {
    name = "Daedalus";
    exec = "INSERT_PATH_HERE";
    desktopName = "Daedalus";
    genericName = "Crypto-Currency Wallet";
    categories = "Application;Network;";
    icon = "INSERT_ICON_PATH_HERE";
  };
  installer = writeScriptBin "daedalus-installer" ''
    #!${stdenv.shell}

    set -e
    set -x
    export PATH=${lib.makeBinPath [ coreutils gnutar xz pv gnused patchelf ]}

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    echo now installing daedalus...

    chmod -R +w "''${DAEDALUS_DIR}/unpack" || true
    rm -rf "''${DAEDALUS_DIR}/unpack" || true

    mkdir -pv "''${DAEDALUS_DIR}/unpack" "''${HOME}/bin/"

    pv ${tarball} | tar -C "''${DAEDALUS_DIR}/unpack" -xJ

    pushd "''${DAEDALUS_DIR}/unpack"
    cp ${iconPath} icon.png
    popd

    chmod -R +w $DAEDALUS_DIR/unpack

    function fixbin {
      patchelf --set-interpreter $DAEDALUS_DIR/installation/lib/ld-linux-x86-64.so.2 --set-rpath $DAEDALUS_DIR/installation/lib "$@"
    }

    function fixlib {
      patchelf --set-rpath $DAEDALUS_DIR/installation/lib/ "$@"
    }

    for x in cardano-launcher cardano-node electron; do
      fixbin $DAEDALUS_DIR/unpack/bin/$x
    done
    for x in $DAEDALUS_DIR/unpack/lib/*.so* $DAEDALUS_DIR/unpack/lib/dri/*.so; do
      if [[ ! "$x" == */ld-linux-x86-64.so.2 ]]; then
        if [ -f "$x" ]; then
          fixlib $x
        fi
      fi
    done

    if [ -d "''${DAEDALUS_DIR}/installation" ]; then
      mv "''${DAEDALUS_DIR}/installation" "''${DAEDALUS_DIR}/garbage"
    fi
    mv "''${DAEDALUS_DIR}/unpack" "''${DAEDALUS_DIR}/installation"
    rm ~/bin/daedalus || true
    ln -sf "''${DAEDALUS_DIR}/installation/bin/daedalus" ~/bin/daedalus
    cat ${desktopItem}/share/applications/Daedalus.desktop | sed \
      -e "s+INSERT_PATH_HERE+''${DAEDALUS_DIR}/installation/bin/daedalus+g" \
      -e "s+INSERT_ICON_PATH_HERE+''${DAEDALUS_DIR}/installation/icon.png+g" \
      > "''${XDG_DATA_HOME}/applications/Daedalus.desktop"

    if [ -d "''${DAEDALUS_DIR}/garbage" ]; then
      echo cleaning up old version...
      chmod -R u+w "''${DAEDALUS_DIR}/garbage"
      rm -rf "''${DAEDALUS_DIR}/garbage"
    fi

    echo installation finished, daedalus is now in "''${DAEDALUS_DIR}/installation"
  '';
  installerBundle = nix-bundle.nix-bootstrap {
    target = "${installer}";
    run = "/bin/daedalus-installer";
    nixUserChrootFlags = "-c -m /home:/home -p HOME";
  };
in installerBundle
