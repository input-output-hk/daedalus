let
  # NOTE: when bumping nixpkgs, also update nixpkgs-src.json and .travis.yml
  pkgs = import (fetchTarball https://github.com/nixos/nixpkgs/archive/fda4b93cd4fd3775408117c380aab0f33737d30f.tar.gz) {};
  nodejs = pkgs.nodejs-8_x;
in

with (import (fetchTarball https://github.com/moretea/yarn2nix/archive/v1.0.0.tar.gz) { inherit pkgs nodejs; });

pkgs.callPackage (
{ stdenv, python }:
mkYarnPackage {
  name = "daedalus";
  src = stdenv.lib.cleanSource ./.;
  installPhase = ''
    npm run build
    mkdir -p $out/bin $out/share/daedalus
    cp -R dist/* $out/share/daedalus
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
}) { }
