{ stdenv, runCommand, writeText, writeScriptBin, fetchurl, fetchFromGitHub, electron,
coreutils, utillinux, procps, network,
frontend, daedalus-bridge, daedalus-installer, daedalus-config,
sandboxed ? false
}:

let
  # closure size TODO list
  # electron depends on cups, which depends on avahi
  daedalus-frontend = writeScriptBin "daedalus-frontend" ''
    #!${stdenv.shell}

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    cd "''${DAEDALUS_DIR}/${network}/"

    exec ${electron}/bin/electron ${frontend}/share/daedalus/main/ "$@"
  '';
  daedalus = writeScriptBin "daedalus" ''
    #!${stdenv.shell}

    set -xe

    ${if sandboxed then ''
    '' else ''
      export PATH="${daedalus-frontend}/bin/:${daedalus-bridge}/bin:$PATH"
    ''}

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export           CLUSTER=${network}
    export           NETWORK=${network}
    export      DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"
    export   DAEDALUS_CONFIG=${if sandboxed then "/nix/var/nix/profiles/profile-${network}/etc" else daedalus-config}
    export        REPORT_URL="$(awk '/reportServer:/ { print $2; }' $DAEDALUS_CONFIG/launcher-config.yaml)"

    mkdir -p "''${DAEDALUS_DIR}/${network}/"{Logs/pub,Secrets}
    cd "''${DAEDALUS_DIR}/${network}/"

    exec ${daedalus-bridge}/bin/cardano-launcher \
      --config ${if sandboxed then "/nix/var/nix/profiles/profile-${network}/etc/launcher-config.yaml" else "${daedalus-config}/launcher-config.yaml"}
  '';
  wrappedConfig = runCommand "launcher-config" {} ''
    mkdir -pv $out/etc/
    cp ${daedalus-config}/* $out/etc/
  '';
in daedalus // {
  cfg = wrappedConfig;
  inherit daedalus-frontend;
}
