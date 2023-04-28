{ inputs, targetSystem, cluster }:

assert targetSystem == "x86_64-windows";

let

  newCommon = import ./new-common.nix { inherit inputs targetSystem cluster; };

  inherit (newCommon) sourceLib oldCode pkgs srcWithoutNix yarn nodejs originalPackageJson commonSources electronVersion;
  inherit (pkgs) lib;
  inherit (oldCode) launcherConfigs;

in rec {

  inherit newCommon oldCode;

  package = preSigning;  # XXX: this is slightly wrong, as not all files are in their final relative paths

  # XXX: Please, use ‘nix run -L .#packages.x86_64-windows.makeSignedInstaller.mainnet’,
  # as the process cannot be done purely, as it requires passing files
  # through `ssh` at the HSM server
  makeSignedInstaller = makeInstaller { signed = true; };

  unsignedInstaller = pkgs.runCommand "win64-installer-${cluster}" {} ''
    ${makeInstaller { signed = false; }}/bin/make-signed-installer
    mkdir $out
    cp -v installers/daedalus-*-*.exe $out/
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
    BUILD_COUNTER = sourceLib.buildCounter;
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
      cp -r node_modules/{\@babel,\@protobufjs,regenerator-runtime,node-fetch,\@trezor,parse-uri,randombytes,safe-buffer,bip66,pushdata-bitcoin,bitcoin-ops,typeforce,varuint-bitcoin,create-hash,blake2b,blakejs,nanoassert,blake2b-wasm,bs58check,bs58,base-x,create-hmac,wif,ms,semver-compare,long,define-properties,object-keys,has,function-bind,es-abstract,has-symbols,json-stable-stringify,cashaddrjs,big-integer,inherits,bchaddrjs,cross-fetch,js-chain-libs-node,bignumber.js,call-bind,get-intrinsic,base64-js,ieee754,util-deprecate,bech32,blake-hash,tiny-secp256k1,bn.js,elliptic,minimalistic-assert,minimalistic-crypto-utils,brorand,hash.js,hmac-drbg,int64-buffer,object.values,bytebuffer,protobufjs,usb-detection,babel-runtime,bindings,brotli,clone,deep-equal,dfa,eventemitter2,file-uri-to-path,fontkit,functions-have-names,has-property-descriptors,has-tostringtag,is-arguments,is-date-object,is-regex,linebreak,node-hid,object-is,pdfkit,png-js,regexp.prototype.flags,restructure,tiny-inflate,unicode-properties,unicode-trie,socks,socks-proxy-agent,ip,smart-buffer,ripple-lib,lodash,jsonschema,ripple-address-codec,ripple-keypairs,ripple-lib-transactionparser,ripple-binary-codec,buffer,decimal.js,debug,agent-base,tslib} $out/resources/app/node_modules

      chmod -R +w $out

      # XXX: remove redundant native modules, and point bindings.js to C:/Program\ Files/Daedalus/*.node instead:
      echo 'Deleting all redundant ‘*.node’ files under to-be-distributed ‘node_modules/’:'
      (
        cd $out/
        find resources/ -name '*.node' -exec rm -vf '{}' ';'
        find resources/app/node_modules -type f '(' -name '*.o' -o -name '*.o.d' -o -name '*.target.mk' -o -name '*.Makefile' -o -name 'Makefile' -o -name 'config.gypi' ')' -exec rm -vf '{}' ';'
        sed -r 's#try: \[#\0 [process.env.DAEDALUS_INSTALL_DIRECTORY, "bindings"],#' -i resources/app/node_modules/bindings/bindings.js
      )

      # TODO: build the distributed ones from source:
      (
        cd $(mktemp -d)
        unzip ${./windows-usb-libs.zip}
        mv build/Debug/*.node $out/
      )
    '';
    dontFixup = true; # TODO: just to shave some seconds, turn back on after everything works
  };

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

  nsisFiles = pkgs.runCommand "nsis-files" {
    buildInputs = [ oldCode.daedalus-installer pkgs.glibcLocales ];
  } ''
    mkdir installers
    cp -vir ${../package.json} package.json
    cd installers

    export LANG=en_US.UTF-8
    cp -v ${launcherConfigs.configFiles}/* .
    make-installer --cardano dummy \
      --os win64 \
      -o $out \
      --cluster ${cluster} \
      --build-rev-short ${sourceLib.buildRevShort} \
      --build-counter ${toString sourceLib.buildCounter} \
      buildkite-cross

    mkdir $out
    cp -v daedalus.nsi uninstaller.nsi $out/
    cp -v ${launcherConfigs.configFiles}/* $out/
    ls -lR $out
  '';

  # the native makensis binary, with cross-compiled windows stubs
  nsis = let
    # TODO, nsis can't cross-compile with the nixpkgs daedalus currently uses
    nsisNixpkgs = pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "nixpkgs";
      rev = "be445a9074f139d63e704fa82610d25456562c3d";
      hash = "sha256-ivcmGg01aeeod0rzjMJ86exUNHHRJu4526rGq9s7rJU=";
    };
    nsisPkgs = import nsisNixpkgs { system = "x86_64-linux"; };
  in nsisPkgs.callPackage ./nsis.nix {};

  wine = pkgs.wine.override { wineBuild = "wine32"; };
  wine64 = pkgs.wine.override { wineBuild = "wineWow"; };

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

  # a cross-compiled fastlist for the ps-list package
  fastlist = pkgs.pkgsCross.mingwW64.callPackage ./fastlist.nix {};

  dlls = pkgs.fetchurl {
    url = "https://s3.eu-central-1.amazonaws.com/daedalus-ci-binaries/DLLs.zip";
    sha256 = "0p6nrf8sg2wgcaf3b1qkbb98bz2dimb7lnshsa93xnmia9m2vsxa";
  };

  preSigning = let
    installDir = oldCode.launcherConfigs.installerConfig.spacedName;
  in pkgs.runCommand "pre-signing" { buildInputs = [ pkgs.unzip ]; } ''
    mkdir $out
    cd $out

    echo '~~~   Preparing files for installer'
    mkdir installers
    cp -vir ${windowsIcons} installers/icons
    cp -vir ${../package.json} package.json
    chmod -R +w installers
    cd installers
    mkdir -pv ../release/win32-x64/
    cp -rv ${daedalusJs} "../release/win32-x64/${installDir}-win32-x64"
    chmod -R +w "../release/win32-x64/${installDir}-win32-x64"
    cp -v ${fastlist}/bin/fastlist.exe "../release/win32-x64/${installDir}-win32-x64/resources/app/dist/main/fastlist.exe"
    ln -s ${../installers/nsis_plugins} nsis_plugins

    mkdir dlls
    pushd dlls
    unzip ${dlls}
    popd
    cp -vr ${oldCode.daedalus-bridge}/bin/* .
    cp -v ${nsisFiles}/{*.yaml,*.json,daedalus.nsi,*.key,*.cert} .
    cp ${unsignedUninstaller}/uninstall.exe .
    if [ -f ${nsisFiles}/block-0.bin ]; then
      cp -v ${nsisFiles}/block-0.bin .
    fi
  '';

  makeInstaller = { signed ? false }: oldCode.pkgs.writeScriptBin "make-signed-installer" ''
    set -euo pipefail

    ${if signed then ''
      # We have to do it impurely:
      cd $(mktemp -d)
      echo "~~~ We’re signing in $PWD:"

      sign_cmd() {
        echo "Signing: ‘$1’…"
        ssh HSM <"$1" >"$1".signed
        mv "$1".signed "$1"
      }
    '' else ''
      sign_cmd() {
        echo "Would sign: ‘$1’"
      }
    ''}

    cp -r ${preSigning}/. ./
    chmod -R +w .

    find . '(' -iname '*.exe' -o -iname '*.dll' -o -iname '*.node' ')' | sort | while IFS= read -r binaryToSign ; do
      sign_cmd "$binaryToSign"
    done

    echo '~~~ Generating installer'
    (
      cd installers/
      ${nsis}/bin/makensis daedalus.nsi -V4
    )

    sign_cmd installers/daedalus-*-*.exe

    echo "Final installer: ‘$(realpath installers/daedalus-*-*.exe)’"
  '';

  windowsSources = {
    electron = pkgs.fetchurl {
      url = "https://github.com/electron/electron/releases/download/v${electronVersion}/electron-v${electronVersion}-win32-x64.zip";
      hash = "sha256-GAhaJQlEf++Iltru6WoS9I+OYKTV7Ez6tE2NWbnYmnI=";
    };
  };

}
