{ pkgs, daedalus, nix-bundle, coreutils, utillinux, procps, lib, gnutar, xz, pv, gnused, bash, bashInteractive, iana-etc, strace, bzip2,
writeScriptBin, fetchFromGitHub, stdenv, makeDesktopItem }:

let
  wrapper = writeScriptBin "daedalus-wrapper" ''
    #!${stdenv.shell}

    set -e
    export PATH=${lib.makeBinPath [
        # used by the scripts&daedalus
        coreutils utillinux procps
        # used by arx for updating
        gnused gnutar bzip2
      ]}

    mkdir /etc/ /bin/
    for x in machine-id resolv.conf; do
      cp /host-etc/$x /etc/$x
    done
    ln -sv ${bash}/bin/sh /bin/sh

    ln -sv ${iana-etc}/etc/protocols /etc/protocols
    ln -sv ${iana-etc}/etc/services /etc/services

    exec ${daedalus}/bin/daedalus
  '';
  namespaceHelper = writeScriptBin "namespaceHelper" ''
    #!/usr/bin/env bash

    set -x
    set -e
    cd "$(dirname "$(realpath "''${BASH_SOURCE[0]}")")"
    mkdir -p etc
    cat /etc/hosts > etc/hosts
    cat /etc/nsswitch.conf > etc/nsswitch.conf

    exec .${nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -m /home:/home -m /etc:/host-etc -p DISPLAY -p HOME -p XAUTHORITY -- ${wrapper}/bin/daedalus-wrapper
  '';
  foo = nix-bundle.nix-bootstrap {
    target = "${wrapper}";
    run = "/bin/daedalus-wrapper";
    nixUserChrootFlags = "-c -m /home:/home -m /etc:/host-etc -p DISPLAY -p HOME -p XAUTHORITY";
  };
  daedalusPackage = pkgs.callPackage (pkgs.path + "/nixos/lib/make-system-tarball.nix") {
    fileName = "daedalusPackage";
    contents = [];
    storeContents = [
      {
        object = namespaceHelper;
        symlink = "/namespaceHelper";
      }
    ];
  };
  desktopItem = makeDesktopItem {
    name = "Daedalus";
    exec = "INSERT_PATH_HERE";
    desktopName = "Daedalus";
    genericName = "Crypto-Currency Wallet";
    categories = "Application;Network;";
    icon = "INSERT_ICON_PATH_HERE";
  };
  iconPath = ./installers/icons/1024x1024.png;
  installer = writeScriptBin "daedalus-installer" ''
    #!${stdenv.shell}

    set -e
    set -x
    export PATH=${lib.makeBinPath [ coreutils gnutar xz pv gnused ]}

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    echo now installing daedalus...

    chmod -R +w "''${DAEDALUS_DIR}/unpack" || true
    rm -rf "''${DAEDALUS_DIR}/unpack" || true

    mkdir -pv "''${DAEDALUS_DIR}/unpack" "''${HOME}/bin/"

    pv ${daedalusPackage}/tarball/daedalusPackage.tar.xz | tar -C "''${DAEDALUS_DIR}/unpack" -xJ

    pushd "''${DAEDALUS_DIR}/unpack"
    rm namespaceHelper
    cp .${builtins.unsafeDiscardStringContext namespaceHelper}/bin/namespaceHelper namespaceHelper
    cp ${iconPath} icon.png
    popd

    if [ -d "''${DAEDALUS_DIR}/installation" ]; then
      mv "''${DAEDALUS_DIR}/installation" "''${DAEDALUS_DIR}/garbage"
    fi
    mv "''${DAEDALUS_DIR}/unpack" "''${DAEDALUS_DIR}/installation"
    rm ~/bin/daedalus || true
    ln -sf "''${DAEDALUS_DIR}/installation/namespaceHelper" ~/bin/daedalus
    cat ${desktopItem}/share/applications/Daedalus.desktop | sed \
      -e "s+INSERT_PATH_HERE+''${DAEDALUS_DIR}/installation/namespaceHelper+g" \
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
in {
  inherit foo installer namespaceHelper installerBundle;
}
