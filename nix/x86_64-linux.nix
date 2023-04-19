{ inputs, targetSystem, cluster }:

assert targetSystem == "x86_64-linux";

let

  newCommon = import ./new-common.nix { inherit inputs targetSystem cluster; };

  inherit (newCommon) sourceLib oldCode pkgs;
  inherit (pkgs) lib;

in rec {

  inherit newCommon oldCode;

  package = daedalus { sandboxed = false; };

  unsignedInstaller = linuxInstaller.wrappedBundle;

  makeSignedInstaller = throw "We don’t sign binary files inside installers for ‘${targetSystem}’, you’re good.";

  # FIXME: for Tullia/Cicero debugging, remove later:
  inherit (sourceLib) buildRev;

  daedalus = { sandboxed }: import ../installers/nix/linux.nix {
    inherit (pkgs) stdenv runCommand writeText writeScriptBin coreutils
      utillinux procps gsettings-desktop-schemas gtk3 hicolor-icon-theme xfce;
    inherit (oldCode) daedalus-bridge daedalus-installer;
    inherit cluster rawapp sandboxed;

    # FIXME: ???
    inherit (oldCode) nodeImplementation launcherConfigs electron;
    linuxClusterBinName = cluster;
  };

  # FIXME: rewrite
  rawapp = import ./old-yarn2nix.nix {
    inherit sourceLib;
    inherit (oldCode.launcherConfigs.installerConfig) spacedName;
    inherit (oldCode.launcherConfigs) launcherConfig;
    inherit cluster;

    inherit (oldCode) yarn nodejs nodePackages cardanoWalletVersion cardanoNodeVersion wine64 windowsIcons;
    inherit (pkgs) lib stdenv python3 python2 python27 nukeReferences fetchzip runCommand fetchurl unzip
      libcap libgcrypt libgpgerror libidn2 libunistring libusb libusb1 libudev lz4 pkgconfig
      writeShellScript xz zlib strace;

    inherit pkgs daedalus;
    inherit (linuxInstaller) iconPath;

    inherit (newCommon) electronVersion patchElectronRebuild;

    win64 = false;
  };

  linuxInstaller = rec {

    installPath = ".daedalus";

    # FIXME: why our own fork?
    nix-bundle-src = pkgs.fetchFromGitHub {
      owner = "input-output-hk"; repo = "nix-bundle";
      rev = "a43e9280628d6e7fcc2f89257106f5262d531bc7";
      sha256 = "10qgincrs8fjdl16mld6lzd69syhyzwx65lcbz4widnkdvhlwh3i";
    };

    nix-bundle = import nix-bundle-src { nixpkgs = pkgs; };
    iconPath = oldCode.launcherConfigs.installerConfig.iconPath;
    linuxClusterBinName = cluster;

    namespaceHelper = pkgs.writeScriptBin "namespaceHelper" ''
      #!/usr/bin/env bash

      set -e

      cd ~/${installPath}/
      mkdir -p etc
      cat /etc/hosts > etc/hosts
      cat /etc/nsswitch.conf > etc/nsswitch.conf
      cat /etc/localtime > etc/localtime
      cat /etc/machine-id > etc/machine-id
      cat /etc/resolv.conf > etc/resolv.conf

      if [ "x$DEBUG_SHELL" == x ]; then
        exec .${nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -e -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -p LANG -p LANGUAGE -p LC_ALL -p LC_MESSAGES -- /nix/var/nix/profiles/profile-${linuxClusterBinName}/bin/enter-phase2 daedalus
      else
        exec .${nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -e -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -p LANG -p LANGUAGE -p LC_ALL -p LC_MESSAGES -- /nix/var/nix/profiles/profile-${linuxClusterBinName}/bin/enter-phase2 bash
      fi
    '';

    desktopItem = pkgs.makeDesktopItem {
      name = "Daedalus-${linuxClusterBinName}";
      exec = "INSERT_PATH_HERE";
      desktopName = "Daedalus ${linuxClusterBinName}";
      genericName = "Crypto-Currency Wallet";
      categories = [ "Application" "Network" ];
      icon = "INSERT_ICON_PATH_HERE";
    };

    postInstall = pkgs.writeScriptBin "post-install" ''
      #!${pkgs.stdenv.shell}

      set -ex


      test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
      export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus/${cluster}"
      mkdir -pv $DAEDALUS_DIR/Logs/pub

      exec 2>&1 > $DAEDALUS_DIR/Logs/pub/post-install.log

      echo "in post-install hook"

      cp -f ${iconPath.large} $DAEDALUS_DIR/icon_large.png
      cp -f ${iconPath.small} $DAEDALUS_DIR/icon.png
      cp -Lf ${namespaceHelper}/bin/namespaceHelper $DAEDALUS_DIR/namespaceHelper
      mkdir -pv ~/.local/bin ''${XDG_DATA_HOME}/applications
      cp -Lf ${namespaceHelper}/bin/namespaceHelper ~/.local/bin/daedalus-${linuxClusterBinName}

      cat ${desktopItem}/share/applications/Daedalus*.desktop | sed \
        -e "s+INSERT_PATH_HERE+''${DAEDALUS_DIR}/namespaceHelper+g" \
        -e "s+INSERT_ICON_PATH_HERE+''${DAEDALUS_DIR}/icon_large.png+g" \
        > "''${XDG_DATA_HOME}/applications/Daedalus-${linuxClusterBinName}.desktop"
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
      daedalus' = daedalus { sandboxed = true; };
    in (import ../installers/nix/nix-installer.nix {
      inherit postInstall preInstall linuxClusterBinName rawapp;
      inherit pkgs;
      installationSlug = installPath;
      installedPackages = [ daedalus' postInstall namespaceHelper daedalus'.cfg oldCode.daedalus-bridge daedalus'.daedalus-frontend xdg-open ];
      nix-bundle = nix-bundle;
    }).installerBundle;

    wrappedBundle = let
      version = (builtins.fromJSON (builtins.readFile ../package.json)).version;
      fn = "daedalus-${version}.${toString sourceLib.buildRevCount}-${linuxClusterBinName}-${sourceLib.buildRevShort}-x86_64-linux.bin";
    in pkgs.runCommand fn {} ''
      mkdir -p $out
      cp ${newBundle} $out/${fn}
    '';

  };

}
