{ stdenv, runCommand, writeText, writeScriptBin, electron3
, coreutils, utillinux, procps, cluster
, rawapp, daedalus-bridge, daedalus-installer
, sandboxed ? false
, nodeImplementation
, jormungandrLib
, launcherConfigs
}:

let
  daedalus-config = runCommand "daedalus-config" {} ''
    mkdir -pv $out
    cd $out
    cp ${writeText "launcher-config.yaml" (builtins.toJSON launcherConfigs.launcherConfig)} $out/launcher-config.yaml
  '';
  # closure size TODO list
  # electron depends on cups, which depends on avahi
  daedalus-frontend = writeScriptBin "daedalus-frontend" ''
    #!${stdenv.shell}

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    cd "''${DAEDALUS_DIR}/${cluster}/"

    exec ${electron3}/bin/electron ${rawapp}/share/daedalus "$@"
  '';
  daedalus = writeScriptBin "daedalus" ''
    #!${stdenv.shell}

    set -xe

    ${if sandboxed then ''
    '' else ''
      export PATH="${daedalus-frontend}/bin/:${daedalus-bridge}/bin:$PATH"
    ''}

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export           CLUSTER=${cluster}
    export      DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
    export   DAEDALUS_CONFIG=${if sandboxed then "/nix/var/nix/profiles/profile-${cluster}/etc" else daedalus-config}

    mkdir -p "''${DAEDALUS_DIR}/${cluster}/"{Logs/pub,Secrets}
    cd "''${DAEDALUS_DIR}/${cluster}/"

    exec ${daedalus-bridge}/bin/cardano-launcher \
      --config ${if sandboxed then "/nix/var/nix/profiles/profile-${cluster}/etc/launcher-config.yaml" else "${daedalus-config}/launcher-config.yaml"}
  '';
  wrappedConfig = runCommand "launcher-config" {} ''
    mkdir -pv $out/etc/
    cp ${daedalus-config}/* $out/etc/
  '';
in daedalus // {
  cfg = wrappedConfig;
  inherit daedalus-frontend;
}
