{ lib, yarn, nodejs, python3, python2, cardanoWalletVersion, cluster, nukeReferences, fetchzip, daedalus, stdenv, win64 ? false, wine64, runCommand, fetchurl, unzip, spacedName, iconPath, launcherConfig, pkgs, python27
, windowsIcons
, libcap
, libgcrypt
, libgpgerror
, libidn2
, libunistring
, libusb
, libusb1
, libudev
, lz4
, pkgconfig
, writeShellScript
, xz
, nodePackages
, zlib
, sourceLib
, cardanoNodeVersion
, electronVersion
, patchElectronRebuild
, originalPackageJson
, yarn2nix
, strace }:
let
  cluster' = launcherConfig.networkName;
  # yarn2nix = import (fetchzip {
  #   # v1.0.0 with a PR to handle duplicate file names between @types/* and original/* – <https://github.com/nix-community/yarn2nix/pull/75>
  #   # TODO: use the version from recent Nixpkgs
  #   url = "https://github.com/nix-community/yarn2nix/archive/276994748d556e0812bb1bc5f92ac095b5da71d2.tar.gz";
  #   sha256 = "1fxiq43w8mfs0aiyj4kazwjl6b829a5r0jbx6bcs3kmil9asq3fg";
  # }) {
  #   inherit pkgs nodejs yarn;
  # };
  newPackage = (originalPackageJson // {
    productName = spacedName;
  }) // lib.optionalAttrs (win64 == false) {
    main = "main/index.js";
  };
  newPackagePath = builtins.toFile "package.json" (builtins.toJSON newPackage);
  electronPath = "https://github.com/electron/electron/releases/download/v${electronVersion}";
  windowsElectron = fetchurl {
    url = "${electronPath}/electron-v${electronVersion}-win32-x64.zip";
    sha256 = "18085a2509447fef8896daeee96a12f48f8e60a4d5ec4cfab44d8d59b9d89a72";
  };
  electronPathHash = builtins.hashString "sha256" electronPath;
  electron-cache = runCommand "electron-cache" {} ''
    # newer style
    mkdir -p $out/${electronPathHash}/
    ln -sv ${windowsElectron} $out/${electronPathHash}/electron-v${electronVersion}-win32-x64.zip
    mkdir $out/httpsgithub.comelectronelectronreleasesdownloadv${electronVersion}electron-v${electronVersion}-win32-x64.zip
    ln -s ${windowsElectron} $out/httpsgithub.comelectronelectronreleasesdownloadv${electronVersion}electron-v${electronVersion}-win32-x64.zip/electron-v${electronVersion}-win32-x64.zip
  '';
  filter = name: type: let
    baseName = baseNameOf (toString name);
    sansPrefix = lib.removePrefix (toString ../.) name;
  in (
      baseName == "package.json" ||
      baseName == "gulpfile.js" ||
      (lib.hasPrefix "/source" sansPrefix) ||
      baseName == ".babelrc" ||
      sansPrefix == "/scripts" ||
      sansPrefix == "/scripts/package.js" ||
      sansPrefix == "/installers" ||
      (lib.hasPrefix "/installers/icons" sansPrefix)
      );
  commonInputs = [
    python27
    python3
    nukeReferences
    strace
    pkgconfig
    libusb
  ];
in
yarn2nix.mkYarnPackage {
  name = "daedalus-js";
  src = lib.cleanSourceWith { inherit filter; src = ../.; name = "daedalus"; };
  CARDANO_WALLET_VERSION = cardanoWalletVersion;
  CARDANO_NODE_VERSION = cardanoNodeVersion;
  CI = "nix";
  NETWORK = cluster';
  BUILD_REV = sourceLib.buildRev;
  BUILD_REV_SHORT = sourceLib.buildRevShort;
  BUILD_REV_COUNT = sourceLib.buildRevCount;
  NODE_ENV = "production";
  BUILDTYPE = "Release";
  extraBuildInputs = commonInputs ++ (if win64 then [ unzip wine64 ] else []);
  installPhase = let
    nukeAllRefs = ''
      # the webpack utils embed the original source paths into map files, so backtraces from the 1 massive index.js can be converted back to multiple files
      # but that causes the derivation to depend on the original inputs at the nix layer, and double the size of the linux installs
      # nuke-refs will just replace all storepaths with an invalid one
      for x in {main,renderer}/{0.,}index.js{,.map} main/preload.js{,.map} main/0.js{,.map} renderer/styles.css.map; do
        nuke-refs $x
      done
    '';
  in if win64 then ''
    # old style
    export ELECTRON_CACHE=${electron-cache}
    # new style
    mkdir -pv home/.cache/
    export HOME=$(realpath home)
    ln -sv ${electron-cache} $HOME/.cache/electron

    # What is this broken symlink used for‽ `electron-packager` fails when there are broken symlinks…
    rm deps/daedalus/daedalus

    cd deps/daedalus/

    cp ${newPackagePath} package.json

    rm -r installers/icons/
    cp -r ${windowsIcons} installers/icons
    chmod -R +w installers/icons

    # TODO: why are the following 2 lines needed?
    mkdir -p installers/icons/${cluster}/${cluster}
    cp ${windowsIcons}/${cluster}/* installers/icons/${cluster}/${cluster}/

    export DEBUG=electron-packager
    yarn --verbose --offline package --win64 --dir $(pwd) --icon installers/icons/${cluster}/${cluster}

    ls -ltrh release/win32-x64/Daedalus*-win32-x64/
    cp -r release/win32-x64/Daedalus*-win32-x64 $out
    pushd $out/resources/app/dist
    ${nukeAllRefs}
    popd
    rm -rf $out/resources/app/{installers,launcher-config.yaml,gulpfile.js,home}

    mkdir -pv $out/resources/app/node_modules
    cp -r $node_modules/{\@babel,\@protobufjs,regenerator-runtime,node-fetch,\@trezor,parse-uri,randombytes,safe-buffer,bip66,pushdata-bitcoin,bitcoin-ops,typeforce,varuint-bitcoin,create-hash,blake2b,blakejs,nanoassert,blake2b-wasm,bs58check,bs58,base-x,create-hmac,wif,ms,semver-compare,long,define-properties,object-keys,has,function-bind,es-abstract,has-symbols,json-stable-stringify,cashaddrjs,big-integer,inherits,bchaddrjs,cross-fetch,js-chain-libs-node,bignumber.js,call-bind,get-intrinsic,base64-js,ieee754,util-deprecate,bech32,blake-hash,blake2,tiny-secp256k1,bn.js,elliptic,minimalistic-assert,minimalistic-crypto-utils,brorand,hash.js,hmac-drbg,int64-buffer,object.values,bytebuffer,protobufjs,usb-detection,babel-runtime,bindings,brotli,clone,deep-equal,dfa,eventemitter2,file-uri-to-path,fontkit,functions-have-names,has-property-descriptors,has-tostringtag,is-arguments,is-date-object,is-regex,linebreak,node-hid,object-is,pdfkit,png-js,regexp.prototype.flags,restructure,tiny-inflate,unicode-properties,unicode-trie,socks,socks-proxy-agent,ip,smart-buffer,ripple-lib,lodash,jsonschema,ripple-address-codec,ripple-keypairs,ripple-lib-transactionparser,ripple-binary-codec,buffer,decimal.js,debug,agent-base,tslib} $out/resources/app/node_modules

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
  '' else throw "code for x86_64-linux from here is no longer used";
  distPhase = ''
    # unused
  '';
  #allowedReferences = [ "out" ];
  #allowedRequisites = [
  #  stdenv.cc.cc.lib
  #  stdenv.cc.cc
  #  stdenv.cc.libc
  #  stdenv.cc.libc.bin
  #  stdenv.cc.libc.dev
  #  libcap.lib
  #  lz4
  #  zlib
  #  xz.out
  #  libgcrypt
  #  libidn2.out
  #  libgpgerror
  #  libunistring
  #  libusb1
  #] ++ stdenv.cc.libc.buildInputs;

  # `yarnPreBuild` is only used in `yarn2nix.mkYarnModules`, not `yarn2nix.mkYarnPackage`:
  yarnPreBuild = ''
    mkdir -p $HOME/.node-gyp/${nodejs.version}
    echo 9 > $HOME/.node-gyp/${nodejs.version}/installVersion
    ln -sfv ${nodejs}/include $HOME/.node-gyp/${nodejs.version}
  '';

  pkgConfig = {
    electron-rebuild = {
      postInstall = ''
        ${patchElectronRebuild}
      '';
    };
  };

  # work around some purity problems in nix
  yarnLock = ../yarn.lock;
  packageJSON = ../package.json;
}
