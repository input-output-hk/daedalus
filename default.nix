let
  itn_clusters = [ "itn_rewards_v1"  "qa" "nightly" "itn_selfnode" ];
  getDefaultBackend = cluster: if (builtins.elem cluster itn_clusters) then "jormungandr" else "cardano";
in
{ target ? builtins.currentSystem
, nodeImplementation ? (getDefaultBackend cluster)
, localLib ? import ./lib.nix { inherit nodeImplementation; }
, config ? {}
, cluster ? "mainnet"
, version ? "versionNotSet"
, buildNum ? null
, dummyInstaller ? false
, signingKeys ? null
, HSMServer ? null
, fudgeConfig ? null
, devShell ? false
, useLocalNode ? false
}:

let
  systemTable = {
    x86_64-windows = builtins.currentSystem;
  };
  crossSystemTable = lib: {
    x86_64-windows = lib.systems.examples.mingwW64;
  };
  system = systemTable.${target} or target;
  pkgs = localLib.iohkNix.getPkgsDefault { inherit system config; };
  pkgsNative = localLib.iohkNix.getPkgsDefault {};
  sources = localLib.sources;
  walletPkgs = import "${sources.cardano-wallet}/nix" {};
  # only used for CLI, to be removed when upgraded to next node version
  nodePkgs = import "${sources.cardano-node}/nix" {};
  shellPkgs = (import "${sources.cardano-shell}/nix/iohk-common.nix").getPkgs {};
  inherit (pkgs.lib) optionalString optional concatStringsSep;
  inherit (pkgs) writeTextFile;
  crossSystem = lib: (crossSystemTable lib).${target} or null;
  # TODO, nsis cant cross-compile with the nixpkgs daedalus currently uses
  nsisNixPkgs = import localLib.sources.nixpkgs-nsis {};
  installPath = ".daedalus";
  needSignedBinaries = (signingKeys != null) || (HSMServer != null);
  buildNumSuffix = if buildNum == null then "" else ("-${builtins.toString buildNum}");
  throwSystem = throw "Unsupported system: ${pkgs.stdenv.hostPlatform.system}";
  ostable.x86_64-windows = "windows";
  ostable.x86_64-linux = "linux";
  ostable.x86_64-darwin = "macos64";
  packages = self: {
    inherit cluster pkgs version target nodeImplementation;
    jormungandrLib = localLib.iohkNix.jormungandrLib;
    cardanoLib = localLib.iohkNix.cardanoLib;
    daedalus-bridge = self.bridgeTable.${nodeImplementation};
    export-wallets = self.cardano-sl.nix-tools.cexes.cardano-wallet.export-wallets;
    db-converter = self.cardano-wallet.db-converter;

    nodejs = pkgs.nodejs-12_x;
    yarnInfo = {
      version = "1.22.4";
      hash = "1l3sv30g61dcn7ls213prcja2y3dqdi5apq9r7yyick295w25npq";
    };
    yarn = (pkgs.yarn.override { inherit (self) nodejs; }).overrideAttrs (old: {
      version = self.yarnInfo.version;
      src = pkgs.fetchFromGitHub {
        owner = "yarnpkg";
        repo = "yarn";
        rev = "v${self.yarnInfo.version}";
        sha256 = self.yarnInfo.hash;
      };
    });

    sources = localLib.sources;
    bridgeTable = {
      jormungandr = self.callPackage ./nix/jormungandr-bridge.nix {};
      cardano = self.callPackage ./nix/cardano-bridge.nix {};
    };
    cardano-wallet = import self.sources.cardano-wallet { inherit system; gitrev = self.sources.cardano-wallet.rev; crossSystem = crossSystem walletPkgs.lib; };
    cardano-wallet-native = import self.sources.cardano-wallet { inherit system; gitrev = self.sources.cardano-wallet.rev; };
    cardano-shell = import self.sources.cardano-shell { inherit system; crossSystem = crossSystem shellPkgs.lib; };
    cardano-cli = (import self.sources.cardano-node { inherit system; crossSystem = crossSystem nodePkgs.lib; }).haskellPackages.cardano-node.components.exes.cardano-cli;
    cardano-node = if useLocalNode
                   then (import self.sources.cardano-node { inherit system; crossSystem = crossSystem nodePkgs.lib; }).haskellPackages.cardano-node.components.exes.cardano-node
                   else self.cardano-wallet.cardano-node;
    cardano-sl = import self.sources.cardano-sl { inherit target; gitrev = self.sources.cardano-sl.rev; };

    # a cross-compiled fastlist for the ps-list package
    fastlist = pkgs.pkgsCross.mingwW64.callPackage ./nix/fastlist.nix {};
    wine = pkgs.wine.override { wineBuild = "wine32"; };
    wine64 = pkgs.wine.override { wineBuild = "wineWow"; };

    dlls = pkgs.fetchurl {
      url = "https://s3.eu-central-1.amazonaws.com/daedalus-ci-binaries/DLLs.zip";
      sha256 = "0p6nrf8sg2wgcaf3b1qkbb98bz2dimb7lnshsa93xnmia9m2vsxa";
    };

    # the native makensis binary, with cross-compiled windows stubs
    nsis = nsisNixPkgs.callPackage ./nix/nsis.nix {};

    launcherConfigs = self.callPackage ./nix/launcher-config.nix {
      inherit (self) jormungandrLib;
      inherit devShell;
      network = cluster;
      os = ostable.${target};
      backend = nodeImplementation;
      runCommandNative = pkgsNative.runCommand;
    };

    itnClustersFile = writeTextFile {
      name = "itn-clusters";
      text = concatStringsSep " " itn_clusters;
    };

    unsignedUnpackedCardano = self.daedalus-bridge; # TODO
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
      cp ${self.signFile "${self.unsignedUnpackedCardano}/bin/jormungandr.exe"} bin/jormungandr.exe
      cp ${self.signFile "${self.unsignedUnpackedCardano}/bin/cardano-wallet-jormungandr.exe"} bin/cardano-wallet-jormungandr.exe
      cp ${self.signFile "${self.unsignedUnpackedCardano}/bin/jcli.exe"} bin/jcli.exe
    '';
    dummyUnpacked = pkgs.runCommand "dummy-unpacked-cardano" {} ''
      mkdir $out
      cd $out
      touch cardano-launcher.exe cardano-node.exe cardano-x509-certificates.exe log-config-prod.yaml configuration.yaml mainnet-genesis.json
    '';

    nsisFiles = pkgs.runCommand "nsis-files" {
      buildInputs = [ self.daedalus-installer pkgs.glibcLocales ];
    } ''
      mkdir installers
      cp -vir ${./package.json} package.json
      cd installers

      echo ${self.daedalus-bridge.wallet-version} > version

      export LANG=en_US.UTF-8
      cp -v ${self.launcherConfigs.configFiles}/* .
      make-installer --${nodeImplementation} dummy --os win64 -o $out --cluster ${cluster} ${optionalString (buildNum != null) "--build-job ${buildNum}"} buildkite-cross

      mkdir $out
      cp -v daedalus.nsi uninstaller.nsi $out/
      cp -v ${self.launcherConfigs.configFiles}/* $out/
      ls -lR $out
    '';

    unsignedUninstaller = pkgs.runCommand "uninstaller" { buildInputs = [ self.nsis self.wine ]; } ''
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
      installDir = self.launcherConfigs.installerConfig.spacedName;
    in pkgs.runCommand "win64-installer-${cluster}" {
      buildInputs = [
        self.daedalus-installer self.nsis pkgs.unzip pkgs.jq self.yaml2json
      ];
    } ''
      echo '~~~   Preparing files for installer'
      mkdir home
      export HOME=$(realpath home)

      mkdir -p $out/{nix-support,cfg-files}
      mkdir installers
      cp -vir ${./installers/dhall} installers/dhall
      cp -vir ${./installers/icons} installers/icons
      cp -vir ${./package.json} package.json
      chmod -R +w installers
      cd installers
      mkdir -pv ../release/win32-x64/
      ${if dummyInstaller then ''mkdir -pv "../release/win32-x64/${installDir}-win32-x64/resources/app/dist/main/"'' else ''cp -rv ${self.rawapp-win64} "../release/win32-x64/${installDir}-win32-x64"''}
      chmod -R +w "../release/win32-x64/${installDir}-win32-x64"
      cp -v ${self.fastlist}/bin/fastlist.exe "../release/win32-x64/${installDir}-win32-x64/resources/app/dist/main/fastlist.exe"
      ln -s ${./installers/nsis_plugins} nsis_plugins

      mkdir dlls
      pushd dlls
      ${if dummyInstaller then "touch foo" else "unzip ${self.dlls}"}
      popd
      cp -v ${self.unpackedCardano}/bin/* .
      cp -v ${self.nsisFiles}/{*.yaml,*.json,daedalus.nsi,*.key,*.cert} .
      cp ${self.uninstaller}/uninstall.exe ../uninstall.exe
      if [ -f ${self.nsisFiles}/block-0.bin ]; then
        cp -v ${self.nsisFiles}/block-0.bin .
      fi
      chmod -R +w .
      ${optionalString (fudgeConfig != null) ''
        set -x
        KEY=$(yaml2json launcher-config.yaml | jq .configuration.key -r)
        config-mutator configuration.yaml ''${KEY} ${toString fudgeConfig.applicationVersion} > temp
        mv temp configuration.yaml
        set +x
      ''}

      echo '~~~   Generating installer'
      makensis daedalus.nsi -V4

      echo '~~~   Copying to $out'
      cp daedalus-*-*-wallet-*-windows*.exe $out/
      cp *.yaml $out/cfg-files/
      echo file installer $out/*.exe > $out/nix-support/hydra-build-products
    '';
    signed-windows-installer = let
      backend_version = self.daedalus-bridge.wallet-version;
      frontend_version = (builtins.fromJSON (builtins.readFile ./package.json)).version;
      fullName = "daedalus-${frontend_version}-cardano-wallet-${backend_version}-${cluster}-windows${buildNumSuffix}.exe"; # must match to packageFileName in make-installer
    in pkgs.runCommand "signed-windows-installer-${cluster}" {} ''
      mkdir $out
      cp -v ${self.signFile "${self.unsigned-windows-installer}/${fullName}"} $out/${fullName}
    '';
    windows-installer = if needSignedBinaries then self.signed-windows-installer else self.unsigned-windows-installer;

    ## TODO: move to installers/nix
    hsDaedalusPkgs = import ./installers {
      inherit (self) daedalus-bridge;
      inherit localLib system;
    };
    daedalus-installer = pkgs.haskell.lib.justStaticExecutables self.hsDaedalusPkgs.daedalus-installer;
    daedalus = self.callPackage ./installers/nix/linux.nix {};
    rawapp = self.callPackage ./yarn2nix.nix {
      inherit buildNum;
      api = "ada";
      apiVersion = self.cardano-sl.daedalus-bridge.version;
      inherit (self.launcherConfigs.installerConfig) spacedName;
      inherit (self.launcherConfigs) launcherConfig;
      inherit cluster;
    };
    rawapp-win64 = self.rawapp.override { win64 = true; };
    source = builtins.filterSource localLib.cleanSourceFilter ./.;
    yaml2json = pkgs.haskell.lib.disableCabalFlag pkgs.haskellPackages.yaml "no-exe";

    electron4 = pkgs.callPackage ./installers/nix/electron.nix {};
    electron8 = self.electron4.overrideAttrs (old: rec {
      name = "electron-${version}";
      version = "8.1.1";
      src = {
        x86_64-linux = pkgs.fetchurl {
          url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-x64.zip";
          sha256 = "0fyqxi5m7vakahq3wls1f8wwx97cpknldd1s06viak2kiv9yk02i";
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
    iconPath = self.launcherConfigs.installerConfig.iconPath;
    # used for name of profile, binary and the desktop shortcut
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
        exec .${self.nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -e -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -p LANG -p LANGUAGE -p LC_ALL -p LC_MESSAGES -- /nix/var/nix/profiles/profile-${self.linuxClusterBinName}/bin/enter-phase2 daedalus
      else
        exec .${self.nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -e -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -p LANG -p LANGUAGE -p LC_ALL -p LC_MESSAGES -- /nix/var/nix/profiles/profile-${self.linuxClusterBinName}/bin/enter-phase2 bash
      fi
    '';
    desktopItem = pkgs.makeDesktopItem {
      name = "Daedalus${if cluster != "mainnet" then "-${self.linuxClusterBinName}" else ""}";
      exec = "INSERT_PATH_HERE";
      desktopName = "Daedalus${if cluster != "mainnet" then " ${self.linuxClusterBinName}" else ""}";
      genericName = "Crypto-Currency Wallet";
      categories = "Application;Network;";
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

      cp -f ${self.iconPath.large} $DAEDALUS_DIR/icon_large.png
      cp -f ${self.iconPath.small} $DAEDALUS_DIR/icon.png
      cp -Lf ${self.namespaceHelper}/bin/namespaceHelper $DAEDALUS_DIR/namespaceHelper
      mkdir -pv ~/.local/bin ''${XDG_DATA_HOME}/applications
      ${pkgs.lib.optionalString (cluster == "mainnet") "cp -Lf ${self.namespaceHelper}/bin/namespaceHelper ~/.local/bin/daedalus"}
      cp -Lf ${self.namespaceHelper}/bin/namespaceHelper ~/.local/bin/daedalus-${self.linuxClusterBinName}

      cat ${self.desktopItem}/share/applications/Daedalus*.desktop | sed \
        -e "s+INSERT_PATH_HERE+''${DAEDALUS_DIR}/namespaceHelper+g" \
        -e "s+INSERT_ICON_PATH_HERE+''${DAEDALUS_DIR}/icon_large.png+g" \
        > "''${XDG_DATA_HOME}/applications/Daedalus${if cluster != "mainnet" then "-${self.linuxClusterBinName}" else ""}.desktop"
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
      inherit (self) postInstall preInstall linuxClusterBinName rawapp;
      inherit pkgs;
      installationSlug = installPath;
      installedPackages = [ daedalus' self.postInstall self.namespaceHelper daedalus'.cfg self.daedalus-bridge daedalus'.daedalus-frontend self.xdg-open ];
      nix-bundle = self.nix-bundle;
    }).installerBundle;
    wrappedBundle = let
      version = (builtins.fromJSON (builtins.readFile ./package.json)).version;
      backend = "cardano-wallet-${nodeImplementation}";
      suffix = if buildNum == null then "" else "-${toString buildNum}";
      fn = "daedalus-${version}-${backend}-${self.linuxClusterBinName}-${target}${suffix}.bin";
    in pkgs.runCommand fn {} ''
      mkdir -p $out
      cp ${self.newBundle} $out/${fn}
    '';
    };
in pkgs.lib.makeScope pkgs.newScope packages
