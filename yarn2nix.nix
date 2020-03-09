{ lib, pkgs, nodejs-8_x, python, api, apiVersion, cluster, buildNum, nukeReferences, fetchzip, daedalus, stdenv, win64 ? false, wine, runCommand, fetchurl }:
let
  nodejs = nodejs-8_x;
  yarn2nix = import (fetchzip {
    url = "https://github.com/moretea/yarn2nix/archive/v1.0.0.tar.gz";
    sha256 = "02bzr9j83i1064r1r34cn74z7ccb84qb5iaivwdplaykyyydl1k8";
  }) { inherit pkgs nodejs; };
  # TODO: these hard-coded values will go away when wallet port
  # selection happens at runtime.
  walletPortMap = {
    mainnet = 8090;
    staging = 8092;
    testnet = 8094;
  };
  dotGitExists = builtins.pathExists ./.git;
  isNix2 = 0 <= builtins.compareVersions builtins.nixVersion "1.12";
  canUseFetchGit = dotGitExists && isNix2;
  origPackage = builtins.fromJSON (builtins.readFile ./package.json);
  nameTable = {
    mainnet = "Daedalus ADA Redemption";
    staging = "Daedalus Staging";
    testnet = "Daedalus Testnet";
  };
  newPackage = (origPackage // {
    productName = nameTable.${if cluster == null then "testnet" else cluster};
  }) // lib.optionalAttrs (win64 == false) {
    main = "main/index.js";
  };
  newPackagePath = builtins.toFile "package.json" (builtins.toJSON newPackage);
  windowsElectronVersion = "3.0.14";
  windowsElectron = fetchurl {
    url = "https://github.com/electron/electron/releases/download/v${windowsElectronVersion}/electron-v${windowsElectronVersion}-win32-x64.zip";
    sha256 = "0cqwjmv1ymwa309v025szs6681f891s6ks653jd5mh55hp1vpn0b";
  };
  checksums = fetchurl {
    url = "https://github.com/electron/electron/releases/download/v${windowsElectronVersion}/SHASUMS256.txt";
    sha256 = "103m5kxgb64clx68qqfvxdz2pah249lk344mjxqj94i83v9bxd2j";
  };
  electron-cache = runCommand "electron-cache" {} ''
    mkdir $out
    ln -s ${windowsElectron} $out/electron-v3.0.14-win32-x64.zip
    ln -s ${checksums} $out/SHASUMS256.txt-3.0.14
  '';
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
in
yarn2nix.mkYarnPackage {
  name = "daedalus-js";
  src = lib.cleanSourceWith { inherit filter; src = ./.; };
  API = api;
  API_VERSION = apiVersion;
  CI = "nix";
  NETWORK = cluster;
  WALLET_PORT = walletPortMap.${cluster};
  BUILD_NUMBER = "${toString buildNum}";
  NODE_ENV = "production";
  extraBuildInputs = if win64 then [ wine nukeReferences ] else [ nukeReferences ];
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
    cp ${daedalus.cfg}/etc/launcher-config.yaml ./launcher-config.yaml
    export ELECTRON_CACHE=${electron-cache}
    mkdir home
    export HOME=$(realpath home)
    cp ${newPackagePath} package.json
    yarn --offline package --win64 --icon installers/icons/${cluster}/${cluster}
    ls -ltrh release/win32-x64/Daedalus*-win32-x64/
    cp -r release/win32-x64/Daedalus*-win32-x64 $out
    pushd $out/resources/app/dist
    ${nukeAllRefs}
    popd
    rm -rf $out/resources/app/{installers,launcher-config.yaml,gulpfile.js,home}
  '' else ''
    cp -v ${daedalus.cfg}/etc/launcher-config.yaml ./launcher-config.yaml
    yarn --offline run build
    mkdir -p $out/bin $out/share/daedalus
    cp -R dist/* $out/share/daedalus
    cp ${newPackagePath} $out/share/daedalus/package.json
    pushd $out/share/daedalus
    ${nukeAllRefs}
    popd
    mkdir -p $out/share/fonts
    ln -sv $out/share/daedalus/renderer/assets $out/share/fonts/daedalus
  '';
  allowedReferences = [ "out" ];
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
  };
  # work around some purity problems in nix
  yarnLock = ./yarn.lock;
  packageJSON = ./package.json;
}
