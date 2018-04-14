{ lib, pkgs, nodejs-8_x, python, api, cluster, nukeReferences, version, fetchzip, daedalus }:
let
  nodejs = nodejs-8_x;
  yarn2nix = import (fetchzip {
    url = "https://github.com/moretea/yarn2nix/archive/v1.0.0.tar.gz";
    sha256 = "02bzr9j83i1064r1r34cn74z7ccb84qb5iaivwdplaykyyydl1k8";
  }) { inherit pkgs nodejs; };
  networkMap = {
    mainnet = "mainnet";
    staging = "testnet";
  };
in
yarn2nix.mkYarnPackage {
  name = "daedalus-js";
  src = if 0 <= builtins.compareVersions builtins.nixVersion "1.12" then builtins.fetchGit ./. else lib.cleanSource ./.;
  API = api;
  NETWORK = networkMap.${cluster};
  DAEDALUS_VERSION = "${version}";
  NODE_ENV = "production";
  installPhase = ''
    cp -vi ${daedalus.cfg}/etc/launcher-config.yaml ./launcher-config.yaml
    npm run build
    mkdir -p $out/bin $out/share/daedalus
    cp -R dist/* $out/share/daedalus
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/main/index.js.map
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/main/0.index.js.map
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/renderer/index.js.map
  '';
  allowedReferences = [];
  yarnPreBuild = ''
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
  };
  # work around some purity problems in nix
  yarnLock = ./yarn.lock;
  packageJSON = ./package.json;
}
