{ target
, nodeImplementation ? "cardano"
, cluster ? "mainnet"
, version ? "versionNotSet"
, dummyInstaller ? false
, signingKeys ? null
, HSMServer ? null
, fudgeConfig ? null
, devShell ? false
, useLocalNode ? false
, topologyOverride ? null
, configOverride ? null
, genesisOverride ? null
, sourceLib
, inputs
}:

let
  system = {
    x86_64-windows = "x86_64-linux"; # Windows can only be cross-built from Linux now
  }.${target} or target;
  pkgs = inputs.nixpkgs.legacyPackages.${system};
  localLib = import ./old-lib.nix { inherit inputs system nodeImplementation; };
  sources = localLib.sources;
  haskell-nix = inputs.cardano-wallet-unpatched.inputs.haskellNix.legacyPackages.${system}.haskell-nix;
  flake-compat = import inputs.cardano-wallet-unpatched.inputs.flake-compat;
  walletFlake =
    if target != "aarch64-darwin"
    then inputs.cardano-wallet-unpatched
    else (flake-compat {
      # FIXME: add patches in `flake.nix` after <https://github.com/NixOS/nix/issues/3920>
      src = pkgs.runCommand "cardano-wallet" {} ''
        cp -r ${inputs.cardano-wallet-unpatched} $out
        chmod -R +w $out
        cd $out
        patch -p1 -i ${./cardano-wallet--enable-aarch64-darwin.patch}
      '';
    }).defaultNix // {
      inherit (inputs.cardano-wallet-unpatched) rev shortRev sourceInfo;
    };
  walletPackages = with walletFlake.hydraJobs; {
    x86_64-windows = linux.windows;
    x86_64-linux = linux.native;
    x86_64-darwin = macos.intel;
    aarch64-darwin = macos.silicon;
  }.${target};
  walletPkgs = walletFlake.legacyPackages.${system}.pkgs;
  cardanoWorldFlake = (flake-compat { src = sources.cardano-world; }).defaultNix.outputs;
  shellPkgs = (import "${sources.cardano-shell}/nix") { inherit system; };
  inherit (pkgs.lib) optionalString;
  crossSystem = lib: {
    x86_64-windows = lib.systems.examples.mingwW64;
  }.${target} or null;
  # TODO, nsis can't cross-compile with the nixpkgs daedalus currently uses
  nsisNixPkgs = import localLib.sources.nixpkgs-nsis { inherit system; };
  needSignedBinaries = (signingKeys != null) || (HSMServer != null);
  ostable.x86_64-windows = "windows";
  ostable.x86_64-linux = "linux";
  ostable.x86_64-darwin = "macos64";
  ostable.aarch64-darwin = "macos64-arm";

  packages = self: {
    inherit walletFlake cardanoWorldFlake cluster pkgs version target nodeImplementation;
    cardanoLib = walletPkgs.cardanoLib;
    daedalus-bridge = self.bridgeTable.${nodeImplementation};

    # nodejs = let
    #   njPath = pkgs.path + "/pkgs/development/web/nodejs";
    #   buildNodeJs = pkgs.callPackage (import (njPath + "/nodejs.nix")) {
    #     python = pkgs.python39;
    #     icu = pkgs.icu68; # can’t build against ICU 69: <https://chromium-review.googlesource.com/c/v8/v8/+/2477751>
    #   };
    # in
    #   buildNodeJs {
    #     enableNpm = true;
    #     version = "14.17.0";
    #     sha256 = "1vf989canwcx0wdpngvkbz2x232yccp7fzs1vcbr60rijgzmpq2n";
    #     patches = pkgs.lib.optional pkgs.stdenv.isDarwin (njPath + "/bypass-xcodebuild.diff");
    #   };

    nodejs = pkgs.nodejs-14_x.overrideAttrs (drv: {
      # XXX: we don’t want `bypass-xcodebuild.diff`, rather we supply
      # the pure `xcbuild` – without that, `blake2` doesn’t build,
      # cf. <https://github.com/NixOS/nixpkgs/blob/29ae6a1f3d7a8886b3772df4dc42a13817875c7d/pkgs/development/web/nodejs/bypass-xcodebuild.diff>
      patches = [];
    });

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
      cardano = self.callPackage ./cardano-bridge.nix {};
    };
    inherit (walletPackages) cardano-wallet;
    inherit (walletPackages) cardano-address;
    inherit (walletPackages) mock-token-metadata-server;
    cardano-shell = import self.sources.cardano-shell { inherit system; crossSystem = crossSystem shellPkgs.lib; };
    local-cluster = if cluster == "selfnode" then walletPackages.local-cluster else null;
    cardano-node = walletPackages.cardano-node;
    cardanoNodeVersion = self.cardano-node.version + "-" + builtins.substring 0 9 self.cardano-node.src.rev;
    cardanoWalletVersion = self.daedalus-bridge.wallet-version + "-" + builtins.substring 0 9 walletFlake.rev;
    cardano-cli = walletPackages.cardano-cli;

    # a cross-compiled fastlist for the ps-list package
    fastlist = pkgs.pkgsCross.mingwW64.callPackage ./fastlist.nix {};
    wine = pkgs.wine.override { wineBuild = "wine32"; };
    wine64 = pkgs.wine.override { wineBuild = "wineWow"; };

    dlls = pkgs.fetchurl {
      url = "https://s3.eu-central-1.amazonaws.com/daedalus-ci-binaries/DLLs.zip";
      sha256 = "0p6nrf8sg2wgcaf3b1qkbb98bz2dimb7lnshsa93xnmia9m2vsxa";
    };

    # the native makensis binary, with cross-compiled windows stubs
    nsis = nsisNixPkgs.callPackage ./nsis.nix {};

    launcherConfigs = self.callPackage ./launcher-config.nix {
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
      cp -vir ${../package.json} package.json
      cd installers

      export LANG=en_US.UTF-8
      cp -v ${self.launcherConfigs.configFiles}/* .
      make-installer --${nodeImplementation'} dummy \
        --os win64 \
        -o $out \
        --cluster ${cluster} \
        --build-rev-short ${sourceLib.buildRevShort} \
        --build-rev-count ${toString sourceLib.buildRevCount} \
        buildkite-cross

      mkdir $out
      cp -v daedalus.nsi uninstaller.nsi $out/
      cp -v ${self.launcherConfigs.configFiles}/* $out/
      ls -lR $out
    '';

    unsignedUninstaller = pkgs.runCommand "uninstaller" { buildInputs = [ self.nsis self.wine ]; } ''
      mkdir home
      export HOME=$(realpath home)

      ln -sv ${../installers/nsis_plugins} nsis_plugins
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
      srcCluster = if builtins.pathExists (../installers/icons + "/${cluster}") then cluster else "mainnet";
    in pkgs.runCommand "windows-icons-${cluster}" { inherit buildInputs; } ''
      mkdir -p $out/${cluster} $out
      cp -r ${../installers/icons + "/${srcCluster}"}/. $out/${cluster}/.
      cp ${../installers/icons/installBanner.bmp} $out/installBanner.bmp
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
      cp -vir ${../package.json} package.json
      chmod -R +w installers
      cd installers
      mkdir -pv ../release/win32-x64/
      ${if dummyInstaller then ''mkdir -pv "../release/win32-x64/${installDir}-win32-x64/resources/app/dist/main/"'' else ''cp -rv ${self.rawapp-win64} "../release/win32-x64/${installDir}-win32-x64"''}
      chmod -R +w "../release/win32-x64/${installDir}-win32-x64"
      cp -v ${self.fastlist}/bin/fastlist.exe "../release/win32-x64/${installDir}-win32-x64/resources/app/dist/main/fastlist.exe"
      ln -s ${../installers/nsis_plugins} nsis_plugins

      mkdir dlls
      pushd dlls
      ${if dummyInstaller then "touch foo" else "unzip ${self.dlls}"}
      popd
      cp -vr ${self.unpackedCardano}/bin/* .
      cp -v ${self.nsisFiles}/{*.yaml,*.json,daedalus.nsi,*.key,*.cert} .
      cp ${self.uninstaller}/uninstall.exe .
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
      backend_version = self.cardanoWalletVersion;
      frontend_version = (builtins.fromJSON (builtins.readFile ../package.json)).version;
      fullName = "daedalus-${frontend_version}.${toString sourceLib.buildRevCount}-${cluster}-${sourceLib.buildRevShort}-x86_64-windows.exe"; # must match to packageFileName in make-installer
    in pkgs.runCommand "signed-windows-installer-${cluster}" {} ''
      mkdir $out
      cp -v ${self.signFile "${self.unsigned-windows-installer}/${fullName}"} $out/${fullName}
    '';
    windows-installer = if needSignedBinaries then self.signed-windows-installer else self.unsigned-windows-installer;

    ## TODO: move to installers/nix
    hsDaedalusPkgs = self.callPackage ../installers {
      inherit (self) daedalus-bridge;
      inherit localLib system;
    };
    daedalus-installer = pkgs.haskell.lib.justStaticExecutables self.hsDaedalusPkgs.daedalus-installer;
    rawapp-win64 = self.callPackage ./old-yarn2nix.nix {
      inherit sourceLib;
      inherit (self.launcherConfigs.installerConfig) spacedName;
      inherit (self.launcherConfigs) launcherConfig;
      inherit cluster;
      win64 = true;
      daedalus = throw "do not use";
      iconPath = throw "do not use";
      inherit (inputs.self.packages.x86_64-windows.internal.mainnet.newCommon) electronVersion patchElectronRebuild;
    };
    source = builtins.filterSource localLib.cleanSourceFilter ../.;
    inherit ((haskell-nix.hackage-package { name = "yaml"; compiler-nix-name = "ghc8107"; cabalProject = ''
      packages: .
      package yaml
        flags: -no-exe
    ''; }).components.exes) yaml2json;

    electron = pkgs.callPackage ../installers/nix/electron.nix {};

    tests = {
      runShellcheck = self.callPackage ../tests/shellcheck.nix { src = ../.;};
    };

  };
in pkgs.lib.makeScope pkgs.newScope packages
