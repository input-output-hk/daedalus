let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? localLib.iohkNix.getPkgs { inherit system config; }
, cluster ? "mainnet"
, version ? "versionNotSet"
, buildNum ? null
}:

let
  installPath = ".daedalus";
  cardanoSL = localLib.cardanoSL { inherit system config; };
  cleanSourceFilter = with pkgs.stdenv;
    name: type: let baseName = baseNameOf (toString name); in ! (
      # Filter out .git repo
      (type == "directory" && baseName == ".git") ||
      # Filter out editor backup / swap files.
      lib.hasSuffix "~" baseName ||
      builtins.match "^\\.sw[a-z]$" baseName != null ||
      builtins.match "^\\..*\\.sw[a-z]$" baseName != null ||

      # Filter out locally generated/downloaded things.
      baseName == "dist" ||
      baseName == "node_modules" ||

      # Filter out the files which I'm editing often.
      lib.hasSuffix ".nix" baseName ||
      lib.hasSuffix ".dhall" baseName ||
      lib.hasSuffix ".hs" baseName ||
      # Filter out nix-build result symlinks
      (type == "symlink" && lib.hasPrefix "result" baseName)
    );
  throwSystem = throw "Unsupported system: ${pkgs.stdenv.hostPlatform.system}";
  packages = self: {
    inherit cluster pkgs version;
    inherit (cardanoSL) daedalus-bridge;
    ## TODO: move to installers/nix
    daedalus-installer = import ./installers/default.nix {
      inherit (cardanoSL) daedalus-bridge;
      inherit localLib system;
    };
    daedalus = self.callPackage ./installers/nix/linux.nix {};
    rawapp = self.callPackage ./yarn2nix.nix {
      inherit buildNum;
      api = "ada";
      apiVersion = cardanoSL.daedalus-bridge.version;
    };
    source = builtins.filterSource cleanSourceFilter ./.;
    yaml2json = pkgs.haskell.lib.disableCabalFlag pkgs.haskellPackages.yaml "no-exe";

    electron4 = pkgs.callPackage ./installers/nix/electron.nix {};
    electron3 = self.electron4.overrideAttrs (old: rec {
      name = "electron-${version}";
      version = "3.1.0";
      src = {
        x86_64-linux = pkgs.fetchurl {
          url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-x64.zip";
          sha256 = "1a8jm01qqadz0ya86ivx2y5zadasx8pi34asp54d7g75hn20p5pp";
        };
      }.${pkgs.stdenv.hostPlatform.system} or throwSystem;
    });

    tests = {
      runFlow = self.callPackage ./tests/flow.nix {};
      runLint = self.callPackage ./tests/lint.nix {};
      runShellcheck = self.callPackage ./tests/shellcheck.nix { src = ./.;};
    };
    nix-bundle = import (pkgs.fetchFromGitHub {
      owner = "matthewbauer";
      repo = "nix-bundle";
      rev = "7f12322399fd87d937355d0fc263d37d798496fc";
      sha256 = "07wnmdadchf73p03wk51abzgd3zm2xz5khwadz1ypbvv3cqlzp5m";
    }) { nixpkgs = pkgs; };
    desktopItem = pkgs.makeDesktopItem {
      name = "Daedalus${if cluster != "mainnet" then "-${cluster}" else ""}";
      exec = "INSERT_PATH_HERE";
      desktopName = "Daedalus${if cluster != "mainnet" then " ${cluster}" else ""}";
      genericName = "Crypto-Currency Wallet";
      categories = "Application;Network;";
      icon = "INSERT_ICON_PATH_HERE";
    };
    iconPath = {
      # the target of these paths must not be a symlink
      demo    = {
        small = ./installers/icons/mainnet/64x64.png;
        large = ./installers/icons/mainnet/1024x1024.png;
      };
      mainnet = {
        small = ./installers/icons/mainnet/64x64.png;
        large = ./installers/icons/mainnet/1024x1024.png;
      };
      staging = {
        small = ./installers/icons/staging/64x64.png;
        large = ./installers/icons/staging/1024x1024.png;
      };
      testnet = {
        small = ./installers/icons/testnet/64x64.png;
        large = ./installers/icons/testnet/1024x1024.png;
      };
    };
    namespaceHelper = pkgs.writeScriptBin "namespaceHelper" ''
      #!/usr/bin/env bash

      set -e

      cd ~/${installPath}/
      mkdir -p etc
      cat /etc/hosts > etc/hosts
      cat /etc/nsswitch.conf > etc/nsswitch.conf
      cat /etc/machine-id > etc/machine-id
      cat /etc/resolv.conf > etc/resolv.conf

      if [ "x$DEBUG_SHELL" == x ]; then
        exec .${self.nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -e -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -- /nix/var/nix/profiles/profile-${cluster}/bin/enter-phase2 daedalus
      else
        exec .${self.nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -e -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -- /nix/var/nix/profiles/profile-${cluster}/bin/enter-phase2 bash
      fi
    '';
    postInstall = pkgs.writeScriptBin "post-install" ''
      #!${pkgs.stdenv.shell}

      set -ex


      test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
      export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus/${cluster}"
      mkdir -pv $DAEDALUS_DIR/Logs/pub

      exec 2>&1 > $DAEDALUS_DIR/Logs/pub/post-install.log

      echo "in post-install hook"

      cp -f ${self.iconPath.${cluster}.large} $DAEDALUS_DIR/icon_large.png
      cp -f ${self.iconPath.${cluster}.small} $DAEDALUS_DIR/icon.png
      cp -Lf ${self.namespaceHelper}/bin/namespaceHelper $DAEDALUS_DIR/namespaceHelper
      mkdir -pv ~/.local/bin ''${XDG_DATA_HOME}/applications
      ${pkgs.lib.optionalString (cluster == "mainnet") "cp -Lf ${self.namespaceHelper}/bin/namespaceHelper ~/.local/bin/daedalus"}
      cp -Lf ${self.namespaceHelper}/bin/namespaceHelper ~/.local/bin/daedalus-${cluster}

      cat ${self.desktopItem}/share/applications/Daedalus*.desktop | sed \
        -e "s+INSERT_PATH_HERE+''${DAEDALUS_DIR}/namespaceHelper+g" \
        -e "s+INSERT_ICON_PATH_HERE+''${DAEDALUS_DIR}/icon_large.png+g" \
        > "''${XDG_DATA_HOME}/applications/Daedalus${if cluster != "mainnet" then "-${cluster}" else ""}.desktop"
    '';
    xdg-open = pkgs.writeScriptBin "xdg-open" ''
      #!${pkgs.stdenv.shell}

      echo -n "xdg-open \"$1\"" > /escape-hatch
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
      daedalus' = self.daedalus.override { sandboxed = true; };
    in (import ./installers/nix/nix-installer.nix {
      inherit (self) postInstall preInstall cluster;
      inherit pkgs;
      installationSlug = installPath;
      installedPackages = [ daedalus' self.postInstall self.namespaceHelper daedalus'.cfg self.daedalus-bridge daedalus'.daedalus-frontend self.xdg-open ];
      nix-bundle = self.nix-bundle;
    }).installerBundle;
  };
in pkgs.lib.makeScope pkgs.newScope packages
