let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, runCommand
, source
, rawapp
, pkgs ? localLib.iohkNix.getPkgs { inherit system config; }
}:
let
  nodejs = pkgs.nodejs-12_x;
  yarn = pkgs.yarn.override { inherit nodejs; };
  lint = runCommand "daedalus-lint-ci" { buildInputs = [ yarn ]; } ''
    set -x
    export NO_UPDATE_NOTIFIER=1
    ln -s ${rawapp.node_modules} node_modules
    cp -a ${source}/. .
    mkdir /tmp/yarn-cache
    yarn --offline --cache-folder /tmp/yarn-cache run lint
    rm -r /tmp/yarn-cache
    EXIT_CODE=$?
    if [ $EXIT_CODE == 0 ]
    then
      echo $EXIT_CODE > $out
      exit 0
    fi
  '';
in lint
