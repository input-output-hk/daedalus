let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:

let
<<<<<<< HEAD
  yarn = pkgs.yarn.override { inherit nodejs; };
  nodejs = pkgs.nodejs-8_x;
  daedalusShell = pkgs.stdenv.mkDerivation {
    name = "daedalus";

    buildInputs = [ nodejs yarn ] ++ (with pkgs; [
      nix bash binutils coreutils curl gnutar
      git python27 curl electron
      nodePackages.node-gyp nodePackages.node-pre-gyp
      gnumake
    ]);
  };
  fixYarnLock = pkgs.stdenv.mkDerivation {
    name = "fix-yarn-lock";
    buildInputs = [ nodejs yarn pkgs.git ];
    shellHook = ''
      git diff > pre-yarn.diff
      yarn
      git diff > post-yarn.diff
      diff pre-yarn.diff post-yarn.diff > /dev/null
      if [ $? != 0 ]
      then
        echo "Changes by yarn have been made. Please commit them."
      else
        echo "No changes were made."
      fi
      rm pre-yarn.diff post-yarn.diff
      exit
    '';
  };
in daedalusShell // { inherit fixYarnLock; }
=======
  daedalusShell = pkgs.stdenv.mkDerivation {
    name = "daedalus";
    passthru = { inherit daedalus; };

    buildInputs = with pkgs; [
      nix bash binutils coreutils curl gnutar
      git python27 curl electron nodejs-8_x
      nodePackages.node-gyp nodePackages.node-pre-gyp
      gnumake yarn
    ];
    shellHook = ''
      yarn install
      ln -svf ${pkgs.electron}/bin/electron ./node_modules/electron/dist/electron
      echo "Instructions:"
      echo "In cardano repo run scripts/launch/demo-nix.sh"
      echo "export CARDANO_TLS_PATH=/path/to/cardano-sl/state-demo/tls/client"
      echo "yarn dev"
    '';
  };
  daedalus = daedalusShell.overrideAttrs (oldAttrs: {
    shellHook = ''
       if [ ! -f "$CARDANO_TLS_PATH/ca.crt" ]
       then
         echo "CARDANO_TLS_PATH must be set"
         exit 1
       fi
      ${oldAttrs.shellHook}
      yarn dev
      exit 0
    '';
  });
in daedalusShell
>>>>>>> release/0.11.1
