{ pkgs, nodejs-8_x, python, api, buildNr, cluster, nukeReferences }:
let
  nodejs = nodejs-8_x;
  yarn2nix = import (fetchTarball https://github.com/moretea/yarn2nix/archive/v1.0.0.tar.gz) { inherit pkgs nodejs; };
  networkMap = {
    mainnet = "mainnet";
    staging = "testnet";
  };
in
yarn2nix.mkYarnPackage {
  name = "daedalus";
  src = builtins.fetchGit ./.;
  API = api;
  NETWORK = networkMap.${cluster};
  DAEDALUS_VERSION = "1.1.0.${buildNr}";
  installPhase = ''
    npm run build
    mkdir -p $out/bin $out/share/daedalus
    cp -R dist/* $out/share/daedalus
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/main/index.js.map
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/renderer/index.js.map
  '';
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
}
