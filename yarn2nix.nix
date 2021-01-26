{ lib, yarn, nodejs, python, api, apiVersion, cluster, buildNum, nukeReferences, fetchzip, daedalus, stdenv, win64 ? false, wine64, runCommand, fetchurl, unzip, spacedName, iconPath, launcherConfig, pkgs, python27
, libcap
, libgcrypt
, libgpgerror
, libidn2
, libunistring
, libusb
, libusb1
, lz4
, pkgconfig
, systemd
, writeShellScriptBin
, xz
, zlib
, strace }:
let
  cluster' = launcherConfig.networkName;
  yarn2nix = import (fetchzip {
    url = "https://github.com/moretea/yarn2nix/archive/v1.0.0.tar.gz";
    sha256 = "02bzr9j83i1064r1r34cn74z7ccb84qb5iaivwdplaykyyydl1k8";
  }) {
    inherit pkgs nodejs yarn;
  };
  dotGitExists = builtins.pathExists ./.git;
  isNix2 = 0 <= builtins.compareVersions builtins.nixVersion "1.12";
  canUseFetchGit = dotGitExists && isNix2;
  origPackage = builtins.fromJSON (builtins.readFile ./package.json);
  newPackage = (origPackage // {
    productName = spacedName;
  }) // lib.optionalAttrs (win64 == false) {
    main = "main/index.js";
  };
  newPackagePath = builtins.toFile "package.json" (builtins.toJSON newPackage);
  windowsElectronVersion = "8.2.2";
  windowsElectron = fetchurl {
    url = "https://github.com/electron/electron/releases/download/v${windowsElectronVersion}/electron-v${windowsElectronVersion}-win32-x64.zip";
    sha256 = "0v9y8qih494k4a5q9s3jgvkdi0nbp60hr0v0w5cxlki79z8gk5ax";
  };
  checksums = fetchurl {
    url = "https://github.com/electron/electron/releases/download/v${windowsElectronVersion}/SHASUMS256.txt";
    sha256 = "1z9wcgqjjany2ny4k771835m190vyp8h5gjbh898mf81mk7h3805";
  };
  electron-cache = runCommand "electron-cache" {} ''
    mkdir $out
    # old style
    ln -s ${windowsElectron} $out/electron-v${windowsElectronVersion}-win32-x64.zip
    ln -s ${checksums} $out/SHASUMS256.txt-${windowsElectronVersion}
    # new style
    mkdir $out/httpsgithub.comelectronelectronreleasesdownloadv${windowsElectronVersion}SHASUMS256.txt
    mkdir $out/httpsgithub.comelectronelectronreleasesdownloadv${windowsElectronVersion}electron-v${windowsElectronVersion}-win32-x64.zip
    ln -s ${windowsElectron} $out/httpsgithub.comelectronelectronreleasesdownloadv${windowsElectronVersion}electron-v${windowsElectronVersion}-win32-x64.zip/electron-v${windowsElectronVersion}-win32-x64.zip
    ln -s ${checksums} $out/httpsgithub.comelectronelectronreleasesdownloadv${windowsElectronVersion}SHASUMS256.txt/SHASUMS256.txt
  '';
  electron-gyp = fetchurl {
    url = "https://www.electronjs.org/headers/v8.2.2/node-v8.2.2-headers.tar.gz";
    sha256 = "sha256-7tzr4FojyIcciQ4Krj0WbnWPqDgNdaTGgEnw0mlI9KM=";
  };
  filter = name: type: let
    baseName = baseNameOf (toString name);
    sansPrefix = lib.removePrefix (toString ./.) name;
  in (
      baseName == "package.json" ||
      baseName == "gulpfile.js" ||
      (lib.hasPrefix "/source" sansPrefix) ||
      (lib.hasPrefix "/flow" sansPrefix) ||
      baseName == ".babelrc" ||
      sansPrefix == "/scripts" ||
      sansPrefix == "/scripts/package.js" ||
      sansPrefix == "/installers" ||
      (lib.hasPrefix "/installers/icons" sansPrefix)
      );
  commonInputs = [
    python27
    nukeReferences
    strace
    pkgconfig
    libusb
  ];
  hack = writeShellScriptBin "node-gyp" ''
    echo gyp wrapper
    $NIX_BUILD_TOP/daedalus/node_modules/electron-rebuild/node_modules/.bin/node-gyp-old "$@" --tarball ${electron-gyp} --nodedir $HOME/.electron-gyp/8.2.2/
  '';
in
yarn2nix.mkYarnPackage {
  name = "daedalus-js";
  src = lib.cleanSourceWith { inherit filter; src = ./.; name = "daedalus"; };
  API = api;
  API_VERSION = apiVersion;
  CI = "nix";
  NETWORK = cluster';
  BUILD_NUMBER = "${toString buildNum}";
  NODE_ENV = "production";
  BUILDTYPE = "Release";
  extraBuildInputs = commonInputs ++ (if win64 then [ unzip wine64 ] else []);
  installPhase = let
    nukeAllRefs = ''
      # the webpack utils embed the original source paths into map files, so backtraces from the 1 massive index.js can be converted back to multiple files
      # but that causes the derivation to depend on the original inputs at the nix layer, and double the size of the linux installs
      # nuke-refs will just replace all storepaths with an invalid one
      for x in {main,renderer}/index.js{,.map} main/preload.js{,.map} main/0.js{,.map} renderer/styles.css.map; do
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

    cp ${newPackagePath} package.json
    mkdir -p installers/icons/${cluster}/${cluster}
    cp ${iconPath.base}/* installers/icons/${cluster}/${cluster}/
    yarn --offline package --win64 --icon installers/icons/${cluster}/${cluster}
    ls -ltrh release/win32-x64/Daedalus*-win32-x64/
    cp -r release/win32-x64/Daedalus*-win32-x64 $out
    pushd $out/resources/app/dist
    ${nukeAllRefs}
    popd
    rm -rf $out/resources/app/{installers,launcher-config.yaml,gulpfile.js,home}

    mkdir -pv $out/resources/app/node_modules
    cp -rv $node_modules/{\@babel,regenerator-runtime,node-fetch,\@trezor,runtypes,parse-uri,randombytes,safe-buffer,bip66,pushdata-bitcoin,bitcoin-ops,typeforce,varuint-bitcoin,bigi,create-hash,merkle-lib,blake2b,nanoassert,blake2b-wasm,bs58check,bs58,base-x,create-hmac,ecurve,wif,ms,keccak,trezor-link,semver-compare,protobufjs-old-fixed-webpack,bytebuffer-old-fixed-webpack,long,object.values,define-properties,object-keys,has,function-bind,es-abstract,has-symbols,json-stable-stringify,tiny-worker,hd-wallet,cashaddrjs,big-integer,queue,inherits,bchaddrjs,cross-fetch,trezor-connect,js-chain-libs-node,bignumber.js} $out/resources/app/node_modules

    cd $out/resources/app/
    unzip ${./nix/windows-usb-libs.zip}
  '' else ''
    mkdir -pv home/.cache/
    export HOME=$(realpath home)
    yarn --offline run build

    mkdir -pv $HOME/.electron-gyp/
    tar -xvf ${electron-gyp} -C $HOME/.electron-gyp
    mv -vi $HOME/.electron-gyp/node_headers $HOME/.electron-gyp/8.2.2/

    ln -sv $HOME/.electron-gyp $HOME/.node-gyp

    #export DEBUG=electron-rebuild

    ls -ltrha $NIX_BUILD_TOP/daedalus/node_modules/
    function dup() {
      cp -vr node_modules/''${1}/ node_modules/''${1}-temp
      rm -v node_modules/''${1}
      mv -v node_modules/''${1}-temp node_modules/''${1}
      chmod -R +w node_modules/''${1}
    }
    pwd
    find -name node_modules
    dup keccak
    dup node-hid
    dup secp256k1
    dup usb
    dup @ledgerhq

    node_modules/.bin/electron-rebuild -w usb --useCache -s --debug

    mkdir -p $out/bin $out/share/daedalus
    cp -R dist/* $out/share/daedalus
    cp ${newPackagePath} $out/share/daedalus/package.json
    pushd $out/share/daedalus
    ${nukeAllRefs}
    popd
    mkdir -p $out/share/fonts
    ln -sv $out/share/daedalus/renderer/assets $out/share/fonts/daedalus
    mkdir -pv $out/share/daedalus/node_modules
    cp -rv $node_modules/{\@babel,regenerator-runtime,node-fetch,\@trezor,runtypes,parse-uri,randombytes,safe-buffer,bip66,pushdata-bitcoin,bitcoin-ops,typeforce,varuint-bitcoin,bigi,create-hash,merkle-lib,blake2b,nanoassert,blake2b-wasm,bs58check,bs58,base-x,create-hmac,ecurve,wif,ms,keccak,trezor-link,semver-compare,protobufjs-old-fixed-webpack,bytebuffer-old-fixed-webpack,long,object.values,define-properties,object-keys,has,function-bind,es-abstract,has-symbols,json-stable-stringify,tiny-worker,hd-wallet,cashaddrjs,big-integer,queue,inherits,bchaddrjs,cross-fetch,trezor-connect,js-chain-libs-node,bignumber.js} $out/share/daedalus/node_modules/
    find $out $NIX_BUILD_TOP -name '*.node'

    mkdir -pv $out/share/daedalus/build
    cp node_modules/usb/build/Debug/usb_bindings.node $out/share/daedalus/build/usb_bindings.node
    cp node_modules/node-hid/build/Debug/HID-hidraw.node $out/share/daedalus/build/HID-hidraw.node
    for file in $out/share/daedalus/build/usb_bindings.node $out/share/daedalus/build/HID-hidraw.node; do
      $STRIP $file
      patchelf --shrink-rpath $file
    done
  '';
  #allowedReferences = [ "out" ];
  #allowedRequisites = [
  #  systemd.lib
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
  yarnPreBuild = ''
    mkdir -p $HOME/.node-gyp/${nodejs.version}
    echo 9 > $HOME/.node-gyp/${nodejs.version}/installVersion
    ln -sfv ${nodejs}/include $HOME/.node-gyp/${nodejs.version}
  '';
  pkgConfig = {
    node-sass = {
      buildInputs = [ python ];
      postInstall = ''
        yarn --offline run build
        rm build/config.gypi
      '';
    };
    flow-bin = {
      postInstall = ''
        flow_ver=${origPackage.devDependencies."flow-bin"}
        patchelf --set-interpreter ${stdenv.cc.libc}/lib/ld-linux-x86-64.so.2 flow-linux64-v$flow_ver/flow
      '';
    };
    electron-rebuild = {
      postInstall = ''
        if [ -d node_modules ]; then
          mv -vi node_modules/.bin/node-gyp node_modules/.bin/node-gyp-old
          ln -sv ${hack}/bin/node-gyp node_modules/.bin/node-gyp
        fi
      '';
    };
  };
  # work around some purity problems in nix
  yarnLock = ./yarn.lock;
  packageJSON = ./package.json;
}
