let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
}:

let
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
