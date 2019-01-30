{ lib, pkgs, nodejs-8_x, python, api, apiVersion, cluster, buildNum, nukeReferences, fetchzip, daedalus, stdenv }:
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
    mainnet = "Daedalus";
    staging = "Daedalus Staging";
    testnet = "Daedalus Testnet";
  };
  newPackage = origPackage // {
    productName = nameTable.${if cluster == null then "testnet" else cluster};
    main = "main/index.js";
  };
  newPackagePath = builtins.toFile "package.json" (builtins.toJSON newPackage);
in
yarn2nix.mkYarnPackage {
  name = "daedalus-js";
  src = if canUseFetchGit then builtins.fetchGit ./. else lib.cleanSource ./.;
  API = api;
  API_VERSION = apiVersion;
  CI = "nix";
  NETWORK = cluster;
  WALLET_PORT = walletPortMap.${cluster};
  BUILD_NUMBER = "${toString buildNum}";
  NODE_ENV = "production";
  installPhase = ''
    cp -v ${daedalus.cfg}/etc/launcher-config.yaml ./launcher-config.yaml
    npm run build
    mkdir -p $out/bin $out/share/daedalus
    cp -R dist/* $out/share/daedalus
    cp ${newPackagePath} $out/share/daedalus/package.json
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/main/index.js.map
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/main/preload.js.map
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/main/0.js.map
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/renderer/index.js.map
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/renderer/styles.css.map
    for x in $out/share/daedalus/renderer/index.js $out/share/daedalus/main/preload.js $out/share/daedalus/main/index.js $out/share/daedalus/main/0.js; do
      ${nukeReferences}/bin/nuke-refs $x
    done
    mkdir -p $out/share/fonts
    ln -sv $out/share/daedalus/renderer/assets $out/share/fonts/daedalus
  '';
  allowedReferences = [ "out" ];
  yarnPreBuild = ''
    set -x
    mkdir -p $HOME/.node-gyp/${nodejs.version}
    echo 9 > $HOME/.node-gyp/${nodejs.version}/installVersion
    ln -sfv ${nodejs}/include $HOME/.node-gyp/${nodejs.version}
  '';
  pkgConfig = {
    node-sass = {
      buildInputs = [ python ];
      postInstall = ''
        npm run build
      '';
    };
    flow-bin = {
      postInstall = ''
      flow_ver=$(${pkgs.jq}/bin/jq .devDependencies.'"flow-bin"' ${newPackagePath} | sed 's/"//g')
        patchelf --set-interpreter ${stdenv.cc.libc}/lib/ld-linux-x86-64.so.2 flow-linux64-v$flow_ver/flow
      '';
    };
  };
  # work around some purity problems in nix
  yarnLock = ./yarn.lock;
  packageJSON = ./package.json;
}
