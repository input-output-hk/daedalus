{ lib, pkgs, nodejs-8_x, python, backend, nukeReferences, version, fetchzip, stdenv, src }:
let
  nodejs = nodejs-8_x;
  yarn2nix = import (fetchzip {
    url = "https://github.com/moretea/yarn2nix/archive/v1.0.0.tar.gz";
    sha256 = "02bzr9j83i1064r1r34cn74z7ccb84qb5iaivwdplaykyyydl1k8";
  }) { inherit pkgs nodejs; };
in
yarn2nix.mkYarnPackage {
  name = "daedalus-frontend";
  inherit src;
  # Provide direct paths to yarn sources so that
  # import-from-derivation works in sandboxed build.
  yarnLock = ../yarn.lock;
  packageJSON = ../package.json;

  API = backend.api;
  API_VERSION = backend.version;
  CI = "nix";
  DAEDALUS_VERSION = version;
  BUILD_NUMBER = toString buildNum;
  NODE_ENV = "production";

  installPhase = ''
    npm run build
    mkdir -p $out/bin $out/share/daedalus
    cp -R dist/* $out/share/daedalus
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/main/index.js.map
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/main/0.index.js.map
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/renderer/index.js.map
    ${nukeReferences}/bin/nuke-refs $out/share/daedalus/renderer/styles.css.map
  '';
  allowedReferences = [];
  yarnPreBuild = ''
    mkdir -p $HOME/.node-gyp/${nodejs.version}
    echo 9 > $HOME/.node-gyp/${nodejs.version}/installVersion
    ln -sfv ${nodejs}/include $HOME/.node-gyp/${nodejs.version}
  '';
  pkgConfig = {
    node-sass = {
      buildInputs = [ python ] ++
        lib.optionals stdenv.isDarwin [ pkgs.bashInteractive pkgs.darwin.cctools ];
      postInstall = "npm run build";
    };
  } // lib.optionalAttrs stdenv.isLinux {
    flow-bin = {
      postInstall = ''
        patchelf --set-interpreter ${stdenv.cc.libc}/lib/ld-linux-x86-64.so.2 flow-linux64-v0.60.1/flow
      '';
    };
  };
}
