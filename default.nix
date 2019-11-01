{ target ? builtins.currentSystem
, nodeImplementation ? "jormungandr"
, localLib ? import ./lib.nix { inherit nodeImplementation; }
, config ? {}
, cluster ? "mainnet"
, version ? "versionNotSet"
, buildNum ? null
, dummyInstaller ? false
, signingKeys ? null
, HSMServer ? null
, fudgeConfig ? null
}:

let
  systemTable = {
    x86_64-windows = builtins.currentSystem;
  };
  crossSystemTable = {
    x86_64-windows = lib.systems.examples.mingwW64;
  };
  system = systemTable.${target} or target;
  pkgs = localLib.iohkNix.getPkgs { inherit system config; };
  crossSystem = crossSystemTable.${target} or null;
  # TODO, nsis cant cross-compile with the nixpkgs daedalus currently uses
  nsisNixPkgs = import localLib.sources.nixpkgs-nsis {};
  installPath = ".daedalus";
  lib = pkgs.lib;
  cardanoSL = localLib.cardanoSL { inherit target; };
  needSignedBinaries = (signingKeys != null) || (HSMServer != null);
  buildNumSuffix = if buildNum == null then "" else ("-${builtins.toString buildNum}");
  throwSystem = throw "Unsupported system: ${pkgs.stdenv.hostPlatform.system}";
  ghcWithCardano = cardanoSL.haskellPackages.ghcWithPackages (ps: [ ps.cardano-sl ps.cardano-sl-x509 ]);
  packages = self: {
    inherit cluster pkgs version;
    inherit (cardanoSL) daedalus-bridge;

    # a cross-compiled fastlist for the ps-list package
    fastlist = pkgs.pkgsCross.mingwW64.callPackage ./fastlist.nix {};

    dlls = pkgs.fetchurl {
      url = "https://s3.eu-central-1.amazonaws.com/daedalus-ci-binaries/DLLs.zip";
      sha256 = "0p6nrf8sg2wgcaf3b1qkbb98bz2dimb7lnshsa93xnmia9m2vsxa";
    };

    # the native makensis binary, with cross-compiled windows stubs
    nsis = nsisNixPkgs.callPackage ./nsis.nix {};

    unsignedUnpackedCardano = cardanoSL.daedalus-bridge;
    unpackedCardano = if dummyInstaller then self.dummyUnpacked else (if needSignedBinaries then self.signedCardano else self.unsignedUnpackedCardano);
    signFile = file: let
      localSigningScript = pkgs.writeScript "signing-script" ''
        #!${pkgs.stdenv.shell}

        exec 3>&1
        exec 1>&2

        export PATH=${pkgs.mono}/bin:$PATH
        PASS=hunter2

        DIR=$(realpath $(mktemp -d))
        cd $DIR
        cp ${file} .
        FILE=$(basename ${file})
        chmod +w $FILE

        # if stdout is a tty, then mono 5.8 will barf over the terminfo files being too new
        # mono 5.16 supports them, but isnt in our current nixpkgs
        # for more info, refer to `mcs/class/corlib/System/TermInfoReader.cs` and `ReadHeader`
        echo $PASS | signcode -spc ${toString signingKeys.spc} -v ${toString signingKeys.pvk} -a sha1 -$ commercial -n "TODO description" -i http://iohk.io -t http://timestamp.verisign.com/scripts/timstamp.dll -tr 10 $FILE | cat
        storePath=$(nix-store --add-fixed sha256 $FILE)
        rm -rf $DIR
        echo $storePath >&3
      '';
      remoteSigningScript = pkgs.writeScript "signing-script" ''
        #!${pkgs.stdenv.shell}

        exec 3>&1
        exec 1>&2

        echo signing "${file}"

        set -e

        DIR=$(realpath $(mktemp -d))
        cd $DIR
        FILE=$(basename ${file})

        cat ${file} | ssh ${HSMServer} > $FILE

        storePath=$(nix-store --add-fixed sha256 $FILE)
        cd /
        rm -rf $DIR
        echo $storePath >&3
      '';
      signingScript = if (HSMServer != null) then remoteSigningScript else localSigningScript;
      # requires --allow-unsafe-native-code-during-evaluation
      res = builtins.exec [ signingScript ];
    in res;
    signedCardano = pkgs.runCommand "signed-daedalus-bridge" {} ''
      cp -r ${self.unsignedUnpackedCardano} $out
      chmod -R +w $out
      cd $out
      rm bin/*.exe
      cp ${self.signFile "${self.unsignedUnpackedCardano}/bin/cardano-launcher.exe"} bin/cardano-launcher.exe
      cp ${self.signFile "${self.unsignedUnpackedCardano}/bin/cardano-node.exe"} bin/cardano-node.exe
      cp ${self.signFile "${self.unsignedUnpackedCardano}/bin/cardano-x509-certificates.exe"} bin/cardano-x509-certificates.exe
      cp ${self.signFile "${self.unsignedUnpackedCardano}/bin/wallet-extractor.exe"} bin/wallet-extractor.exe
    '';
    dummyUnpacked = pkgs.runCommand "dummy-unpacked-cardano" {} ''
      mkdir $out
      cd $out
      touch cardano-launcher.exe cardano-node.exe cardano-x509-certificates.exe log-config-prod.yaml configuration.yaml mainnet-genesis.json
    '';

    nsisFiles = pkgs.runCommand "nsis-files" { buildInputs = [ self.daedalus-installer pkgs.glibcLocales ]; } ''
      mkdir installers
      cp -vir ${./package.json} package.json
      cp -vir ${./installers/dhall} installers/dhall
      cd installers
      cp -vi ${self.unpackedCardano}/version version

      export LANG=en_US.UTF-8
      make-installer --os win64 -o $out --cluster ${cluster} ${lib.optionalString (buildNum != null) "--build-job ${buildNum}"} buildkite-cross

      mkdir $out
      cp daedalus.nsi uninstaller.nsi launcher-config.yaml wallet-topology.yaml $out/
    '';

    unsignedUninstaller = pkgs.runCommand "uninstaller" { buildInputs = [ self.nsis pkgs.winePackages.minimal ]; } ''
      mkdir home
      export HOME=$(realpath home)

      ln -sv ${./installers/nsis_plugins} nsis_plugins
      cp ${self.nsisFiles}/uninstaller.nsi .

      makensis uninstaller.nsi -V4

      wine tempinstaller.exe /S
      mkdir $out
      mv -v $HOME/.wine/drive_c/uninstall.exe $out/uninstall.exe
    '';
    signedUninstaller = pkgs.runCommand "uninstaller-signed" {} ''
      mkdir $out
      cp ${self.signFile "${self.unsignedUninstaller}/uninstall.exe"} $out/uninstall.exe
    '';
    uninstaller = if needSignedBinaries then self.signedUninstaller else self.unsignedUninstaller;

    unsigned-windows-installer = let
      mapping = {
        mainnet = "Daedalus";
        staging = "Daedalus Staging";
        testnet = "Daedalus Testnet";
      };
      installDir = mapping.${cluster};
    in pkgs.runCommand "win64-installer-${cluster}" {
      buildInputs = [
        self.daedalus-installer self.nsis pkgs.unzip pkgs.jq self.yaml2json
      ] ++ lib.optional (fudgeConfig != null) self.configMutator;
    } ''
      mkdir home
      export HOME=$(realpath home)

      mkdir -p $out/{nix-support,cfg-files}
      mkdir installers
      cp ${./cardano-sl-src.json} cardano-sl-src.json
      cp -vir ${./installers/dhall} installers/dhall
      cp -vir ${./installers/icons} installers/icons
      cp -vir ${./package.json} package.json
      chmod -R +w installers
      cd installers
      mkdir -pv ../release/win32-x64/
      ${if dummyInstaller then ''mkdir -pv "../release/win32-x64/${installDir}-win32-x64/resources/app/dist/main/"'' else ''cp -r ${self.rawapp-win64} "../release/win32-x64/${installDir}-win32-x64"''}
      chmod -R +w "../release/win32-x64/${installDir}-win32-x64"
      cp -v ${self.fastlist}/bin/fastlist.exe "../release/win32-x64/${installDir}-win32-x64/resources/app/dist/main/fastlist.exe"
      ln -s ${./installers/nsis_plugins} nsis_plugins

      mkdir dlls
      pushd dlls
      ${if dummyInstaller then "touch foo" else "unzip ${self.dlls}"}
      popd
      cp -v ${self.unpackedCardano}/{bin,config}/* .
      cp ${self.uninstaller}/uninstall.exe ../uninstall.exe
      cp -v ${self.nsisFiles}/{daedalus.nsi,wallet-topology.yaml,launcher-config.yaml} .
      chmod -R +w .
      ${lib.optionalString (fudgeConfig != null) ''
        set -x
        KEY=$(yaml2json launcher-config.yaml | jq .configuration.key -r)
        config-mutator configuration.yaml ''${KEY} ${toString fudgeConfig.applicationVersion} > temp
        mv temp configuration.yaml
        set +x
      ''}

      makensis daedalus.nsi -V4

      cp daedalus-*-cardano-sl-*-windows*.exe $out/
      cp *.yaml $out/cfg-files/
      echo file installer $out/*.exe > $out/nix-support/hydra-build-products
    '';
    signed-windows-installer = let
      backend_version = lib.removeSuffix "\n" (builtins.readFile "${self.unpackedCardano}/version"); # TODO, get from a nix expr
      frontend_version = (builtins.fromJSON (builtins.readFile ./package.json)).version;
      fullName = "daedalus-${frontend_version}-cardano-sl-${backend_version}-${cluster}-windows${buildNumSuffix}.exe"; # must match to packageFileName in make-installer
    in pkgs.runCommand "signed-windows-installer-${cluster}" {} ''
      mkdir $out
      cp -v ${self.signFile "${self.unsigned-windows-installer}/${fullName}"} $out/${fullName}
    '';
    windows-installer = if needSignedBinaries then self.signed-windows-installer else self.unsigned-windows-installer;

    ## TODO: move to installers/nix
    hsDaedalusPkgs = import ./installers {
      #inherit (cardanoSL) daedalus-bridge;
      inherit localLib system;
    };
    daedalus-installer = pkgs.haskell.lib.justStaticExecutables self.hsDaedalusPkgs.daedalus-installer;
    daedalus = self.callPackage ./installers/nix/linux.nix {};
    configMutator = pkgs.runCommand "configMutator" { buildInputs = [ ghcWithCardano ]; } ''
      cp ${./ConfigMutator.hs} ConfigMutator.hs
      mkdir -p $out/bin/
      ghc ConfigMutator.hs -o $out/bin/config-mutator
    '';
    rawapp = self.callPackage ./yarn2nix.nix {
      inherit buildNum;
      api = "ada";
      apiVersion = cardanoSL.daedalus-bridge.version;
      wine = pkgs.wine.override { wineBuild = "wine32"; };
    };
    rawapp-win64 = self.rawapp.override { win64 = true; };
    source = builtins.filterSource localLib.cleanSourceFilter ./.;
    yaml2json = pkgs.haskell.lib.disableCabalFlag pkgs.haskellPackages.yaml "no-exe";

    electron4 = pkgs.callPackage ./installers/nix/electron.nix {};
    electron3 = self.electron4.overrideAttrs (old: rec {
      name = "electron-${version}";
      version = "3.0.14";
      src = {
        x86_64-linux = pkgs.fetchurl {
          url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-x64.zip";
          sha256 = "0wha13dbb8553h9c7kvpnrjj5c6wizr441s81ynmkfbfybg697p7";
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
      cat /etc/localtime > etc/localtime
      cat /etc/machine-id > etc/machine-id
      cat /etc/resolv.conf > etc/resolv.conf

      if [ "x$DEBUG_SHELL" == x ]; then
        exec .${self.nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -e -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -p LANG -p LANGUAGE -p LC_ALL -p LC_MESSAGES -- /nix/var/nix/profiles/profile-${cluster}/bin/enter-phase2 daedalus
      else
        exec .${self.nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -e -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -p LANG -p LANGUAGE -p LC_ALL -p LC_MESSAGES -- /nix/var/nix/profiles/profile-${cluster}/bin/enter-phase2 bash
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
      inherit (self) postInstall preInstall cluster rawapp;
      inherit pkgs;
      installationSlug = installPath;
      installedPackages = [ daedalus' self.postInstall self.namespaceHelper daedalus'.cfg self.daedalus-bridge daedalus'.daedalus-frontend self.xdg-open ];
      nix-bundle = self.nix-bundle;
    }).installerBundle;
  };
in pkgs.lib.makeScope pkgs.newScope packages
