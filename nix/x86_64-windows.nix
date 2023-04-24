{ inputs, targetSystem, cluster }:

assert targetSystem == "x86_64-windows";

let

  newCommon = import ./new-common.nix { inherit inputs targetSystem cluster; };

  inherit (newCommon) sourceLib oldCode pkgs srcWithoutNix yarn nodejs originalPackageJson commonSources electronVersion;
  inherit (pkgs) lib;
  inherit (oldCode) nodeImplementation launcherConfigs;

  dummyInstaller = false;
  fudgeConfig = null;
  signingKeys = null;
  HSMServer = null;

in rec {

  inherit newCommon oldCode;

  package = unsignedInstaller; # FIXME: this is wrong

  unsignedInstaller = unsigned-windows-installer;

  makeSignedInstaller = oldCode.pkgs.writeScriptBin "make-signed-installer" ''
    echo >&2 'fatal: not yet implemented'
    exit 1
  '';

  # They’re initially the same as Linux when cross-compiling for Windows:
  node_modules = inputs.self.packages.x86_64-linux.internal.${cluster}.node_modules;

  electron-cache = pkgs.runCommand "electron-cache" {} ''
    # newer style
    mkdir -p $out/${commonSources.electronCacheHash}/
    ln -sv ${windowsSources.electron} $out/${commonSources.electronCacheHash}/electron-v${electronVersion}-win32-x64.zip
    mkdir $out/httpsgithub.comelectronelectronreleasesdownloadv${electronVersion}electron-v${electronVersion}-win32-x64.zip
    ln -s ${windowsSources.electron} $out/httpsgithub.comelectronelectronreleasesdownloadv${electronVersion}electron-v${electronVersion}-win32-x64.zip/electron-v${electronVersion}-win32-x64.zip
  '';

  daedalusJs = pkgs.stdenv.mkDerivation {
    name = "daedalus-js";
    src = srcWithoutNix;
    nativeBuildInputs = [ yarn nodejs wine64 ]
      ++ (with pkgs; [ python3 pkgconfig unzip ]);
    buildInputs = with pkgs; [ libusb ];
    CARDANO_WALLET_VERSION = oldCode.cardanoWalletVersion;
    CARDANO_NODE_VERSION = oldCode.cardanoNodeVersion;
    CI = "nix";
    NETWORK = launcherConfigs.launcherConfig.networkName;
    BUILD_REV = sourceLib.buildRev;
    BUILD_REV_SHORT = sourceLib.buildRevShort;
    BUILD_REV_COUNT = sourceLib.buildRevCount;
    NODE_ENV = "production";
    BUILDTYPE = "Release";
    configurePhase = newCommon.setupCacheAndGypDirs + ''
      # Grab all cached `node_modules` from above:
      cp -r ${node_modules}/. ./
      chmod -R +w .
    '';
    patchedPackageJson = pkgs.writeText "package.json" (builtins.toJSON (
      pkgs.lib.recursiveUpdate originalPackageJson {
        productName = launcherConfigs.installerConfig.spacedName;
      }
    ));
    buildPhase = ''
      # old style
      export ELECTRON_CACHE=${electron-cache}
      # new style
      mkdir -pv $HOME/.cache/
      ln -sv ${electron-cache} $HOME/.cache/electron

      cp $patchedPackageJson package.json

      rm -r installers/icons/
      cp -r ${windowsIcons} installers/icons
      chmod -R +w installers/icons

      # TODO: why are the following 2 lines needed?
      mkdir -p installers/icons/${cluster}/${cluster}
      cp ${windowsIcons}/${cluster}/* installers/icons/${cluster}/${cluster}/

      export DEBUG=electron-packager
      yarn --verbose --offline package --win64 --dir $(pwd) --icon installers/icons/${cluster}/${cluster}
    '';
    installPhase = ''
      set -x

      ls -ltrh release/win32-x64/Daedalus*-win32-x64/
      cp -r release/win32-x64/Daedalus*-win32-x64 $out

      # XXX: the webpack utils embed the original source paths into map files, which causes the derivation
      # to depend on the original inputs at the nix layer, and double the size of the linux installs.
      # this will just replace all storepaths with an invalid one:
      (
        cd $out/resources/app/dist
        for x in {main,renderer}/{0.,}index.js{,.map} main/preload.js{,.map} main/0.js{,.map} renderer/styles.css.map; do
          ${pkgs.nukeReferences}/bin/nuke-refs $x
        done
      )

      rm -rf $out/resources/app/{installers,launcher-config.yaml,gulpfile.js,home}

      mkdir -pv $out/resources/app/node_modules
      cp -r node_modules/{\@babel,\@protobufjs,regenerator-runtime,node-fetch,\@trezor,parse-uri,randombytes,safe-buffer,bip66,pushdata-bitcoin,bitcoin-ops,typeforce,varuint-bitcoin,create-hash,blake2b,blakejs,nanoassert,blake2b-wasm,bs58check,bs58,base-x,create-hmac,wif,ms,semver-compare,long,define-properties,object-keys,has,function-bind,es-abstract,has-symbols,json-stable-stringify,cashaddrjs,big-integer,inherits,bchaddrjs,cross-fetch,js-chain-libs-node,bignumber.js,call-bind,get-intrinsic,base64-js,ieee754,util-deprecate,bech32,blake-hash,blake2,tiny-secp256k1,bn.js,elliptic,minimalistic-assert,minimalistic-crypto-utils,brorand,hash.js,hmac-drbg,int64-buffer,object.values,bytebuffer,protobufjs,usb-detection,babel-runtime,bindings,brotli,clone,deep-equal,dfa,eventemitter2,file-uri-to-path,fontkit,functions-have-names,has-property-descriptors,has-tostringtag,is-arguments,is-date-object,is-regex,linebreak,node-hid,object-is,pdfkit,png-js,regexp.prototype.flags,restructure,tiny-inflate,unicode-properties,unicode-trie,socks,socks-proxy-agent,ip,smart-buffer,ripple-lib,lodash,jsonschema,ripple-address-codec,ripple-keypairs,ripple-lib-transactionparser,ripple-binary-codec,buffer,decimal.js,debug,agent-base,tslib} $out/resources/app/node_modules

      cd $out/resources/app/
      unzip ${./windows-usb-libs.zip}

      # Investigate why this is needed:
      chmod -R +w $out
      mkdir -p $out/resources/app/node_modules/usb-detection/build
      cp $out/resources/app/build/Debug/detection.node $out/resources/app/node_modules/usb-detection/build
      mkdir -p $out/resources/app/node_modules/node-hid/build
      cp $out/resources/app/build/Debug/HID.node $out/resources/app/node_modules/node-hid/build
      mkdir -p $out/resources/app/node_modules/usb/build
      cp $out/resources/app/build/Debug/usb_bindings.node $out/resources/app/node_modules/usb/build
    '';
    dontFixup = true; # TODO: just to shave some seconds, turn back on after everything works
  };

  # a cross-compiled fastlist for the ps-list package
  fastlist = pkgs.pkgsCross.mingwW64.callPackage ./fastlist.nix {};
  wine = pkgs.wine.override { wineBuild = "wine32"; };
  wine64 = pkgs.wine.override { wineBuild = "wineWow"; };

  dlls = pkgs.fetchurl {
    url = "https://s3.eu-central-1.amazonaws.com/daedalus-ci-binaries/DLLs.zip";
    sha256 = "0p6nrf8sg2wgcaf3b1qkbb98bz2dimb7lnshsa93xnmia9m2vsxa";
  };

  needSignedBinaries = (signingKeys != null) || (HSMServer != null);

  # TODO, nsis can't cross-compile with the nixpkgs daedalus currently uses
  nsisNixPkgs = import oldCode.sources.nixpkgs-nsis { system = "x86_64-linux"; };

  # the native makensis binary, with cross-compiled windows stubs
  nsis = nsisNixPkgs.callPackage ./nsis.nix {};

  unsignedUnpackedCardano = oldCode.daedalus-bridge; # TODO

  unpackedCardano = if dummyInstaller then dummyUnpacked else (if needSignedBinaries then signedCardano else unsignedUnpackedCardano);

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
        cp ${signFile "${unsignedUnpackedCardano}/bin/${bin}"} bin/${bin}
      '';
    in __concatStringsSep "\n" (map signAndCopy launcherConfigs.installerConfig.installerWinBinaries);
  in pkgs.runCommand "signed-daedalus-bridge" {} ''
    cp -r ${unsignedUnpackedCardano} $out
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
    buildInputs = [ oldCode.daedalus-installer pkgs.glibcLocales ];
  } ''
    mkdir installers
    cp -vir ${../package.json} package.json
    cd installers

    export LANG=en_US.UTF-8
    cp -v ${launcherConfigs.configFiles}/* .
    make-installer --${nodeImplementation'} dummy \
      --os win64 \
      -o $out \
      --cluster ${cluster} \
      --build-rev-short ${sourceLib.buildRevShort} \
      --build-rev-count ${toString sourceLib.buildRevCount} \
      buildkite-cross

    mkdir $out
    cp -v daedalus.nsi uninstaller.nsi $out/
    cp -v ${launcherConfigs.configFiles}/* $out/
    ls -lR $out
  '';

  unsignedUninstaller = pkgs.runCommand "uninstaller" { buildInputs = [ nsis wine ]; } ''
    mkdir home
    export HOME=$(realpath home)

    ln -sv ${../installers/nsis_plugins} nsis_plugins
    cp ${nsisFiles}/uninstaller.nsi .

    makensis uninstaller.nsi -V4

    wine tempinstaller.exe /S
    mkdir $out
    mv -v $HOME/.wine/drive_c/uninstall.exe $out/uninstall.exe
  '';

  signedUninstaller = pkgs.runCommand "uninstaller-signed" {} ''
    mkdir $out
    cp ${signFile "${unsignedUninstaller}/uninstall.exe"} $out/uninstall.exe
  '';

  uninstaller = if needSignedBinaries then signedUninstaller else unsignedUninstaller;

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
    installDir = oldCode.launcherConfigs.installerConfig.spacedName;
  in pkgs.runCommand "win64-installer-${cluster}" {
    buildInputs = [
      oldCode.daedalus-installer nsis pkgs.unzip pkgs.jq yaml2json
    ];
  } ''
    echo '~~~   Preparing files for installer'
    mkdir home
    export HOME=$(realpath home)

    mkdir -p $out/{nix-support,cfg-files}
    mkdir installers
    cp -vir ${windowsIcons} installers/icons
    cp -vir ${../package.json} package.json
    chmod -R +w installers
    cd installers
    mkdir -pv ../release/win32-x64/
    ${if dummyInstaller then ''mkdir -pv "../release/win32-x64/${installDir}-win32-x64/resources/app/dist/main/"'' else ''cp -rv ${daedalusJs} "../release/win32-x64/${installDir}-win32-x64"''}
    chmod -R +w "../release/win32-x64/${installDir}-win32-x64"
    cp -v ${fastlist}/bin/fastlist.exe "../release/win32-x64/${installDir}-win32-x64/resources/app/dist/main/fastlist.exe"
    ln -s ${../installers/nsis_plugins} nsis_plugins

    mkdir dlls
    pushd dlls
    ${if dummyInstaller then "touch foo" else "unzip ${dlls}"}
    popd
    cp -vr ${unpackedCardano}/bin/* .
    cp -v ${nsisFiles}/{*.yaml,*.json,daedalus.nsi,*.key,*.cert} .
    cp ${uninstaller}/uninstall.exe .
    if [ -f ${nsisFiles}/block-0.bin ]; then
      cp -v ${nsisFiles}/block-0.bin .
    fi
    chmod -R +w .
    ${pkgs.lib.optionalString (fudgeConfig != null) ''
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
    backend_version = oldCode.cardanoWalletVersion;
    frontend_version = (builtins.fromJSON (builtins.readFile ../package.json)).version;
    fullName = "daedalus-${frontend_version}.${toString sourceLib.buildRevCount}-${cluster}-${sourceLib.buildRevShort}-x86_64-windows.exe"; # must match to packageFileName in make-installer
  in pkgs.runCommand "signed-windows-installer-${cluster}" {} ''
    mkdir $out
    cp -v ${signFile "${unsigned-windows-installer}/${fullName}"} $out/${fullName}
  '';

  windows-installer = if needSignedBinaries then signed-windows-installer else unsigned-windows-installer;

  haskell-nix = inputs.cardano-wallet-unpatched.inputs.haskellNix.legacyPackages.x86_64-linux.haskell-nix;

  inherit ((haskell-nix.hackage-package { name = "yaml"; compiler-nix-name = "ghc8107"; cabalProject = ''
    packages: .
    package yaml
      flags: -no-exe
  ''; }).components.exes) yaml2json;

  windowsSources = {
    electron = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${electronVersion}/electron-v${electronVersion}-win32-x64.zip";
      hash = "sha256-GAhaJQlEf++Iltru6WoS9I+OYKTV7Ez6tE2NWbnYmnI=";
    };
  };

}
