{ cluster ? "mainnet" }:
let
  pkgs = import (import ./fetchNixpkgs.nix (builtins.fromJSON (builtins.readFile ./nixpkgs-src.json))) { config = {}; overlays = []; };
  packages = self: {
    inherit pkgs;
    master_config = {
      daedalus_darwin_url = "https://update-cardano-mainnet.iohk.io/Daedalus-installer-1.1.0.408.pkg";
      cardano_rev = "eef095";
      daedalus_hash = "1b0iy6c5nlsa2n2ylcc2kfm8ykkmdp6ncnx4lwwvc6f0662cf175";
      cardano_hash = "151qjqswrcscr2afsc6am4figw09hyxr80nd3dv3c35dvp2xx4rp";
    };
    inherit cluster;
    cardanoSrc = pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "cardano-sl";
      rev = self.master_config.cardano_rev;
      sha256 = self.master_config.cardano_hash;
    };
    cardanoPkgs = import self.cardanoSrc {
      gitrev = self.cardanoSrc.rev;
      config = {
        packageOverrides = pkgs: {
          rocksdb = pkgs.rocksdb.overrideDerivation (drv: {
            outputs = [ "dev" "out" "static" ];
            postInstall = ''
              ${drv.postInstall}
              mkdir -pv $static/lib/
              mv -vi $out/lib/librocksdb.a $static/lib/
            '';
          });
        };
      };
    };
    daedalus = self.callPackage ./linux.nix {};
    bundle = self.callPackage ./nix-bundle.nix {};
    rawapp = self.callPackage ./rawapp.nix {};
    tarball = self.callPackage ./tarball.nix {};
    tarballInstaller = self.callPackage ./tarball-installer.nix {};
    nix-bundle = import (pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "nix-bundle";
      rev = "630e89d1d16083";
      sha256 = "1s9vzlsfxd2ym8jzv2p64j6jlwr9cmir45mb12yzzjr4dc91xk8x";
    }) { nixpkgs = pkgs; };
    desktopItem = pkgs.makeDesktopItem {
      name = "Daedalus";
      exec = "INSERT_PATH_HERE";
      desktopName = "Daedalus";
      genericName = "Crypto-Currency Wallet";
      categories = "Application;Network;";
      icon = "INSERT_ICON_PATH_HERE";
    };
    iconPath = ./installers/icons/1024x1024.png;
    namespaceHelper = pkgs.writeScriptBin "namespaceHelper" ''
      #!/usr/bin/env bash

      set -ex
      cd ~/daedalus-install/
      mkdir -p etc
      cat /etc/hosts > etc/hosts
      cat /etc/nsswitch.conf > etc/nsswitch.conf
      cat /etc/machine-id > etc/machine-id
      cat /etc/resolv.conf > etc/resolv.conf
      exec .${self.nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -- /nix/var/nix/profiles/profile/bin/enter-phase2 daedalus
    '';
    postInstall = pkgs.writeScriptBin "post-install" ''
      #!${pkgs.stdenv.shell}
      test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
      export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus/mainnet"
      mkdir -pv $DAEDALUS_DIR
      cp -f ${self.iconPath} $DAEDALUS_DIR/icon.png
      cp -Lf ${self.namespaceHelper}/bin/namespaceHelper $DAEDALUS_DIR/namespaceHelper

      cat ${self.desktopItem}/share/applications/Daedalus.desktop | sed \
        -e "s+INSERT_PATH_HERE+''${DAEDALUS_DIR}/namespaceHelper+g" \
        -e "s+INSERT_ICON_PATH_HERE+''${DAEDALUS_DIR}/icon.png+g" \
        > "''${XDG_DATA_HOME}/applications/Daedalus.desktop"
    '';
    newBundle = (import ./nix-installer.nix {
      installationSlug = "daedalus-install";
      installedPackages = [ self.daedalus self.postInstall self.namespaceHelper ];
      postInstall = self.postInstall;
    }).installerBundle;
    configFiles = with self; pkgs.runCommand "cardano-config" {} ''
      mkdir -pv $out
      cd $out
      cp -vi ${cardanoPkgs.cardano-sl.src + "/configuration.yaml"} configuration.yaml
      cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} mainnet-genesis-dryrun-with-stakeholders.json
      cp -vi ${cardanoPkgs.cardano-sl.src + "/mainnet-genesis.json"} mainnet-genesis.json
      cp -vi ${cardanoPkgs.cardano-sl.src + "/../log-configs/daedalus.yaml"} daedalus.yaml
      cp -vi ${topologyFile} topology.yaml
    '';
    topologyFile = self.topologies.${cluster};
    topologies.mainnet = pkgs.writeText "topology.yaml" ''
      wallet:
        relays:
          [
            [
              { host: relays.cardano-mainnet.iohk.io }
            ]
          ]
        valency: 1
        fallbacks: 7
    '';
  };
in pkgs.lib.makeScope pkgs.newScope packages
