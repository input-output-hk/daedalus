{ self, pkgs }:

let
  installPath = ".daedalus";
  icons = {
    mainnet = ./icons/mainnet/1024x1024.png;
    staging = ./icons/staging.iconset/icon_512x512.png;
    testnet = ./icons/testnet.iconset/icon_512x512.png;
  };

  packages = super: self: {
    ## TODO: move to installers/nix
    daedalusLinux = self.callPackage ./nix/linux.nix {};
    nix-bundle = import (pkgs.fetchFromGitHub {
      owner = "matthewbauer";
      repo = "nix-bundle";
      rev = "496f2b524743da67717e4533745394575c6aab1f";
      sha256 = "0p9hsrbc1b0i4aipwnl4vxjsayc5m865xhp8q139ggaxq7xd0lps";
    }) { nixpkgs = pkgs; };
    desktopItem = pkgs.makeDesktopItem {
      name = "Daedalus${if self.network != "mainnet" then "-${self.network}" else ""}";
      exec = "INSERT_PATH_HERE";
      desktopName = "Daedalus${if self.network != "mainnet" then " ${self.network}" else ""}";
      genericName = "Crypto-Currency Wallet";
      categories = "Application;Network;";
      icon = "INSERT_ICON_PATH_HERE";
    };
    iconPath = icons.${self.network};
    namespaceHelper = pkgs.writeScriptBin "namespaceHelper" ''
      #!/usr/bin/env bash

      set -e

      cd ~/${installPath}/
      mkdir -p etc
      cat /etc/hosts > etc/hosts
      cat /etc/nsswitch.conf > etc/nsswitch.conf
      cat /etc/machine-id > etc/machine-id
      cat /etc/resolv.conf > etc/resolv.conf
      exec .${self.nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -- /nix/var/nix/profiles/profile-${self.network}/bin/enter-phase2 daedalus
    '';
    postInstall = pkgs.writeScriptBin "post-install" ''
      #!${pkgs.stdenv.shell}

      set -ex


      test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
      export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus/${self.network}"
      mkdir -pv $DAEDALUS_DIR/Logs/pub

      exec 2>&1 > $DAEDALUS_DIR/Logs/pub/post-install.log

      echo "in post-install hook"

      cp -f ${self.iconPath} $DAEDALUS_DIR/icon.png
      cp -Lf ${self.namespaceHelper}/bin/namespaceHelper $DAEDALUS_DIR/namespaceHelper
      mkdir -pv ~/.local/bin ''${XDG_DATA_HOME}/applications
      cp -Lf ${self.namespaceHelper}/bin/namespaceHelper ~/.local/bin/daedalus
      cp -Lf ${self.namespaceHelper}/bin/namespaceHelper ~/.local/bin/daedalus-${self.network}

      cat ${self.desktopItem}/share/applications/Daedalus*.desktop | sed \
        -e "s+INSERT_PATH_HERE+''${DAEDALUS_DIR}/namespaceHelper+g" \
        -e "s+INSERT_ICON_PATH_HERE+''${DAEDALUS_DIR}/icon.png+g" \
        > "''${XDG_DATA_HOME}/applications/Daedalus${if self.network != "mainnet" then "-${self.network}" else ""}.desktop"
    '';
    preInstall = pkgs.writeText "pre-install" ''
      if grep sse4 /proc/cpuinfo -q; then
        echo 'SSE4 check pass'
      else
        echo "ERROR: your cpu lacks SSE4 support, cardano will not work"
        exit 1
      fi
    '';
    newBundle = let
      daedalus' = self.daedalusLinux.override { sandboxed = true; };
    in (import ./nix/nix-installer.nix {
      inherit (self) postInstall preInstall network;
      installationSlug = installPath;
      installedPackages = [ daedalus' self.postInstall self.namespaceHelper daedalus'.cfg self.daedalus-bridge daedalus'.daedalus-frontend ];
      nix-bundle = self.nix-bundle;
    }).installerBundle // {
      name = "daedalus-${self.version}-cardano-sl-${self.daedalus-bridge.version}.bin";
      inherit (self) network version;
    };
  };

in
  (self.overrideScope packages).newBundle
