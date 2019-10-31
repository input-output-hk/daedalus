{ system ? builtins.currentSystem
, config ? {}
, nodeImplementation ? "jormungandr"
, localLib ? import ./lib.nix { inherit nodeImplementation; }
, pkgs ? localLib.iohkNix.getPkgs { inherit system config; }
, cluster ? "demo"
, systemStart ? null
, autoStartBackend ? systemStart != null
, walletExtraArgs ? []
, allowFaultInjection ? false
, purgeNpmCache ? false
}:

let
  daedalusPkgs = import ./. { inherit cluster; target = system; };
  hostPkgs = import pkgs.path { config = {}; overlays = []; };
  yarn = pkgs.yarn.override { inherit nodejs; };
  nodejs = pkgs.nodejs-12_x;
  fullExtraArgs = walletExtraArgs ++ pkgs.lib.optional allowFaultInjection "--allow-fault-injection";
  launcherConfig' = "${daedalusPkgs.daedalus.cfg}/etc/launcher-config.yaml";
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
  # This has all the dependencies of daedalusShell, but no shellHook allowing hydra
  # to evaluate it.
  daedalusShellBuildInputs = [
      nodejs yarn
      localLib.cardanoWallet
      localLib.cardanoNode
      localLib.jcli
    ] ++ (with pkgs; [
      nix bash binutils coreutils curl gnutar
      git python27 curl jq
      nodePackages.node-gyp nodePackages.node-pre-gyp
      gnumake
      chromedriver
    ] ++ (localLib.optionals autoStartBackend [
      daedalusPkgs.daedalus-bridge
    ]) ++ (localLib.optionals (pkgs.stdenv.hostPlatform.system != "x86_64-darwin") [
      daedalusPkgs.electron3
      winePackages.minimal
    ])
    );
  buildShell = pkgs.stdenv.mkDerivation {
    name = "daedalus-build";
    buildInputs = daedalusShellBuildInputs;
  };
  daedalusShell = pkgs.stdenv.mkDerivation (rec {
    buildInputs = daedalusShellBuildInputs;
    name = "daedalus";
    buildCommand = "touch $out";
    LAUNCHER_CONFIG = launcherConfig';
    DAEDALUS_CONFIG = "${daedalusPkgs.daedalus.cfg}/etc/";
    DAEDALUS_INSTALL_DIRECTORY = "./";
    DAEDALUS_DIR = DAEDALUS_INSTALL_DIRECTORY;
    CLUSTER = cluster;
    NODE_EXE = if nodeImplementation == "jormungandr" then "cardano-wallet-jormungandr" else "cardano-wallet-http-bridge";
    CLI_EXE = if nodeImplementation == "jormungandr" then "jcli" else "";
    NODE_IMPLEMENTATION = nodeImplementation;
    shellHook = let
      secretsDir = if pkgs.stdenv.isLinux then "Secrets" else "Secrets-1.0";
    in ''
      warn() {
         (echo "###"; echo "### WARNING:  $*"; echo "###") >&2
      }

      ${localLib.optionalString pkgs.stdenv.isLinux "export XDG_DATA_HOME=$HOME/.local/share"}
      cp -f ${daedalusPkgs.iconPath.${cluster}.small} $DAEDALUS_INSTALL_DIRECTORY/icon.png

      # These links will only occur to binaries that exist for the
      # specific build config
      ln -svf $(type -P jormungandr)
      ln -svf $(type -P cardano-wallet-jormungandr)
      ln -svf $(type -P jcli)

      export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -I${nodejs}/include/node"
      ${localLib.optionalString purgeNpmCache ''
        warn "purging all NPM/Yarn caches"
        rm -rf node_modules
        yarn cache clean
        npm cache clean --force
        ''
      }
      yarn install
      ${pkgs.lib.optionalString (pkgs.stdenv.hostPlatform.system != "x86_64-darwin") ''
        ln -svf ${daedalusPkgs.electron3}/bin/electron ./node_modules/electron/dist/electron
        ln -svf ${pkgs.chromedriver}/bin/chromedriver ./node_modules/electron-chromedriver/bin/chromedriver
      ''}
    '';
  });
  daedalus = daedalusShell.overrideAttrs (oldAttrs: {
    shellHook = ''
      ${oldAttrs.shellHook}
      yarn dev
      exit 0
    '';
  });
  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = let
      inherit (localLib.iohkNix) niv;
      inherit (localLib) cardanoWallet jormungandr jcli;
    in [ niv cardanoWallet jormungandr jcli ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${pkgs.figlet}/bin/figlet -f banner -c \
      | ${pkgs.lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package

      "
    '';
  };
in daedalusShell // { inherit fixYarnLock buildShell devops; }
