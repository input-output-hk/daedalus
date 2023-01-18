{ target ? builtins.currentSystem
, localLibSystem ? builtins.currentSystem
, nodeImplementation ? "cardano"
, localLib ? import ./lib.nix { inherit nodeImplementation; system = localLibSystem; }
, cluster ? "mainnet"
, version ? "versionNotSet"
, buildNum ? null
, dummyInstaller ? false
, signingKeys ? null
, HSMServer ? null
, fudgeConfig ? null
, devShell ? false
, useLocalNode ? false
, topologyOverride ? null
, configOverride ? null
, genesisOverride ? null
}:

let
  systemTable = {
    x86_64-windows = "x86_64-linux"; # Windows can only be cross-built from Linux now
  };
  crossSystemTable = lib: {
    x86_64-windows = lib.systems.examples.mingwW64;
  };
  system = systemTable.${target} or target;
  config = {
    packageOverrides = super: {
      systemd = super.systemd.overrideAttrs ({ patches ? [], ... }: {
        patches = patches ++ [ ./nix/systemd.patch ];
      });
    };
  };
  pkgs = import sources.nixpkgs { inherit system config; };
  sources = localLib.sources // {
    cardano-wallet =
      if target != "aarch64-darwin"
      then localLib.sources.cardano-wallet
      else pkgs.runCommand "cardano-wallet" {} ''
        cp -r ${localLib.sources.cardano-wallet} $out
        chmod -R +w $out
        cd $out
        patch -p1 -i ${./nix/cardano-wallet--enable-aarch64-darwin.patch}
      '';
  };
  haskell-nix = walletFlake.inputs.haskellNix.legacyPackages.${system}.haskell-nix;
  flake-compat = import sources.flake-compat;
  walletFlake = (flake-compat  { src = sources.cardano-wallet; }).defaultNix;
  walletPackages = with walletFlake.hydraJobs; {
    x86_64-windows = linux.windows;
    x86_64-linux = linux.native;
    x86_64-darwin = macos.intel;
    aarch64-darwin = macos.silicon;
  }.${target};
  walletPkgs = walletFlake.legacyPackages.${system}.pkgs;
  cardanoWorldFlake = (flake-compat { src = sources.cardano-world; }).defaultNix.outputs;
  # only used for CLI, to be removed when upgraded to next node version
  nodePkgs = import "${sources.cardano-node}/nix" {};
  shellPkgs = (import "${sources.cardano-shell}/nix") { inherit system; };
  inherit (pkgs.lib) optionalString;
  crossSystem = lib: (crossSystemTable lib).${target} or null;
  # TODO, nsis can't cross-compile with the nixpkgs daedalus currently uses
  nsisNixPkgs = import localLib.sources.nixpkgs-nsis {};
  installPath = ".daedalus";
  needSignedBinaries = (signingKeys != null) || (HSMServer != null);
  buildNumSuffix = if buildNum == null then "" else ("-${builtins.toString buildNum}");
  throwSystem = throw "Unsupported system: ${pkgs.stdenv.hostPlatform.system}";
  ostable.x86_64-windows = "windows";
  ostable.x86_64-linux = "linux";
  ostable.x86_64-darwin = "macos64";
  ostable.aarch64-darwin = "macos64-arm";

  packages = self: {
    inherit walletFlake cardanoWorldFlake cluster pkgs version target nodeImplementation;
    cardanoLib = walletPkgs.cardanoLib;
    daedalus-bridge = self.bridgeTable.${nodeImplementation};

    nodejs = let
      njPath = pkgs.path + "/pkgs/development/web/nodejs";
      buildNodeJs = pkgs.callPackage (import (njPath + "/nodejs.nix")) {
        python = pkgs.python3;
        icu = pkgs.icu68; # can’t build against ICU 69: <https://chromium-review.googlesource.com/c/v8/v8/+/2477751>
      };
    in
      buildNodeJs {
        enableNpm = true;
        version = "14.17.0";
        sha256 = "1vf989canwcx0wdpngvkbz2x232yccp7fzs1vcbr60rijgzmpq2n";
        patches = pkgs.lib.optional pkgs.stdenv.isDarwin (njPath + "/bypass-xcodebuild.diff");
      };

    nodePackages = pkgs.nodePackages.override { nodejs = self.nodejs; };
    yarnInfo = {
      version = "1.22.4";
      hash = "1l3sv30g61dcn7ls213prcja2y3dqdi5apq9r7yyick295w25npq";
    };
    yarn = (pkgs.yarn.override { inherit (self) nodejs; }) /*.overrideAttrs (old: {
      version = self.yarnInfo.version;
      src = pkgs.fetchFromGitHub {
        owner = "yarnpkg";
        repo = "yarn";
        rev = "v${self.yarnInfo.version}";
        sha256 = self.yarnInfo.hash;
      };
    })*/;

    sources = localLib.sources;
    bridgeTable = {
      cardano = self.callPackage ./nix/cardano-bridge.nix {};
    };
    inherit (walletPackages) cardano-wallet;
    inherit (walletPackages) cardano-address;
    inherit (walletPackages) mock-token-metadata-server;
    cardano-shell = import self.sources.cardano-shell { inherit system; crossSystem = crossSystem shellPkgs.lib; };
    local-cluster = if cluster == "selfnode" then walletPackages.local-cluster else null;
    cardano-node-cluster = let
      # Test wallets with known mnemonics
      walletTestGenesisYaml = (self.sources.cardano-wallet + "/lib/shelley/test/data/cardano-node-shelley/genesis.yaml");
      walletTestGenesisJson = pkgs.runCommand "yaml-to-json" { buildInputs = [self.yaml2json]; } ''
        yaml2json ${walletTestGenesisYaml} > $out
      '';
      initialFundsAttrs = (__fromJSON (__readFile walletTestGenesisJson)).initialFunds;
      # Funds required to register pools
      clusterFunds = import (self.sources.cardano-node + "/nix/supervisord-cluster/initial-funds.nix");
      customConfig = {
        initialFunds = clusterFunds // __foldl' (s: x: s // x) {} initialFundsAttrs;
      };
    in (import self.sources.cardano-node { inherit system customConfig; crossSystem = crossSystem nodePkgs.lib; }).cluster;
    cardano-node = if useLocalNode
                   then (import self.sources.cardano-node { inherit system; crossSystem = crossSystem nodePkgs.lib; }).cardano-node
                   else walletPackages.cardano-node;
    cardano-cli = if useLocalNode
                   then (import self.sources.cardano-node { inherit system; crossSystem = crossSystem nodePkgs.lib; }).haskellPackages.cardano-cli
                   else walletPackages.cardano-cli;
    darwin-launcher = self.callPackage ./nix/darwin-launcher.nix {};

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
      inherit devShell topologyOverride configOverride genesisOverride system;
      network = cluster;
      os = ostable.${target};
      backend = nodeImplementation;
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
        # mono 5.16 supports them, but isn't in our current nixpkgs
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
    signedCardano = let
      copySignedBinaries = let
        signAndCopy = bin: ''
          cp ${self.signFile "${self.unsignedUnpackedCardano}/bin/${bin}"} bin/${bin}
        '';
      in __concatStringsSep "\n" (map signAndCopy self.launcherConfigs.installerConfig.installerWinBinaries);
    in pkgs.runCommand "signed-daedalus-bridge" {} ''
      cp -r ${self.unsignedUnpackedCardano} $out
      chmod -R +w $out
      cd $out
      rm bin/*.exe
      ${copySignedBinaries}
    '';
    dummyUnpacked = pkgs.runCommand "dummy-unpacked-cardano" {} ''
      mkdir $out
      cd $out
      touch cardano-launcher.exe cardano-node.exe cardano-x509-certificates.exe log-config-prod.yaml configuration.yaml mainnet-genesis.json
    '';

    nsisFiles = let
      nodeImplementation' = "${nodeImplementation}";
    in pkgs.runCommand "nsis-files" {
      buildInputs = [ self.daedalus-installer pkgs.glibcLocales ];
    } ''
      mkdir installers
      cp -vir ${./package.json} package.json
      cd installers

      echo ${self.daedalus-bridge.wallet-version} > version

      export LANG=en_US.UTF-8
      cp -v ${self.launcherConfigs.configFiles}/* .
      make-installer --${nodeImplementation'} dummy --os win64 -o $out --cluster ${cluster} ${optionalString (buildNum != null) "--build-job ${buildNum}"} buildkite-cross

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

    windowsIcons = let
      buildInputs = with pkgs; [ imagemagick ];
      # Allow fallback to `mainnet` if cluster’s icons don’t exist:
      srcCluster = if builtins.pathExists (./installers/icons + "/${cluster}") then cluster else "mainnet";
    in pkgs.runCommand "windows-icons-${cluster}" { inherit buildInputs; } ''
      mkdir -p $out/${cluster} $out
      cp -r ${./installers/icons + "/${srcCluster}"}/. $out/${cluster}/.
      cp ${./installers/icons/installBanner.bmp} $out/installBanner.bmp
      cd $out/${cluster}
      rm *.ico *.ICO || true   # XXX: just in case
      for f in *.png ; do
        # XXX: these sizes are too large for the ICO format:
        if [ "$f" == 1024x1024.png ] || [ "$f" == 512x512.png ] ; then continue ; fi
        convert "$f" "''${f%.png}.ico"
      done
      convert 16x16.png 24x24.png 32x32.png 48x48.png 64x64.png 128x128.png 256x256.png ${cluster}.ico
    '';

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
      cp -vir ${self.windowsIcons} installers/icons
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
      cp -vr ${self.unpackedCardano}/bin/* .
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
      cp daedalus-*-*.exe $out/
      cp *.yaml $out/cfg-files/
      echo file installer $out/*.exe > $out/nix-support/hydra-build-products
    '';
    signed-windows-installer = let
      backend_version = self.daedalus-bridge.wallet-version;
      frontend_version = (builtins.fromJSON (builtins.readFile ./package.json)).version;
      fullName = "daedalus-${frontend_version}-${cluster}${buildNumSuffix}-x86_64-windows.exe"; # must match to packageFileName in make-installer
    in pkgs.runCommand "signed-windows-installer-${cluster}" {} ''
      mkdir $out
      cp -v ${self.signFile "${self.unsigned-windows-installer}/${fullName}"} $out/${fullName}
    '';
    windows-installer = if needSignedBinaries then self.signed-windows-installer else self.unsigned-windows-installer;

    ## TODO: move to installers/nix
    hsDaedalusPkgs = self.callPackage ./installers {
      inherit (self) daedalus-bridge;
      inherit localLib system;
    };
    daedalus-installer = pkgs.haskell.lib.justStaticExecutables self.hsDaedalusPkgs.daedalus-installer;
    daedalus = self.callPackage ./installers/nix/linux.nix {};
    rawapp = self.callPackage ./yarn2nix.nix {
      inherit buildNum;
      api = "ada";
      apiVersion = self.daedalus-bridge.wallet-version;
      inherit (self.launcherConfigs.installerConfig) spacedName;
      inherit (self.launcherConfigs) launcherConfig;
      inherit cluster;
    };
    rawapp-win64 = self.rawapp.override { win64 = true; };
    source = builtins.filterSource localLib.cleanSourceFilter ./.;
    inherit ((haskell-nix.hackage-package { name = "yaml"; compiler-nix-name = "ghc8107"; cabalProject = ''
      packages: .
      package yaml
        flags: -no-exe
    ''; }).components.exes) yaml2json;

    electron = pkgs.callPackage ./installers/nix/electron.nix {};

    tests = {
      runShellcheck = self.callPackage ./tests/shellcheck.nix { src = ./.;};
    };
    nix-bundle = import sources.nix-bundle { nixpkgs = pkgs; };
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
      name = "Daedalus-${self.linuxClusterBinName}";
      exec = "INSERT_PATH_HERE";
      desktopName = "Daedalus ${self.linuxClusterBinName}";
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
      cp -Lf ${self.namespaceHelper}/bin/namespaceHelper ~/.local/bin/daedalus-${self.linuxClusterBinName}

      cat ${self.desktopItem}/share/applications/Daedalus*.desktop | sed \
        -e "s+INSERT_PATH_HERE+''${DAEDALUS_DIR}/namespaceHelper+g" \
        -e "s+INSERT_ICON_PATH_HERE+''${DAEDALUS_DIR}/icon_large.png+g" \
        > "''${XDG_DATA_HOME}/applications/Daedalus-${self.linuxClusterBinName}.desktop"
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
      suffix = if buildNum == null then "" else "-${toString buildNum}";
      fn = "daedalus-${version}-${self.linuxClusterBinName}${suffix}-x86_64-linux.bin";
    in pkgs.runCommand fn {} ''
      mkdir -p $out
      cp ${self.newBundle} $out/${fn}
    '';
    };
in pkgs.lib.makeScope pkgs.newScope packages
