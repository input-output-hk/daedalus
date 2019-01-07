{ stdenv, callPackage, runCommand, writeText, writeScriptBin, fetchurl, fetchFromGitHub, electron,
coreutils, utillinux, procps, cluster,
rawapp, daedalus-bridge, daedalus-installer,
sandboxed ? false
}:

let
  daedalus-config = runCommand "daedalus-config" {} ''
    mkdir -pv $out
    ## TODO: we don't need all of the genesis files (even if file names sound cool),
    ##       but the choice would have to be made in the Dhall-generated files,
    ##       splitting the dep chain further:
    cp -v ${daedalus-bridge}/config/* $out
    cd $out
    ${daedalus-installer}/bin/make-installer --out-dir "." --cluster ${cluster} config "${daedalus-installer.src}/dhall" "."
  '';
  throwSystem = throw "Unsupported system: ${stdenv.hostPlatform.system}";
  electron4 = callPackage ./electron.nix {};
  electron_3_0_13 = electron4.overrideAttrs (old: rec {
    name = "electron-${version}";
    version = "3.0.13";
    src = {
      x86_64-linux = fetchurl {
        url = "https://github.com/electron/electron/releases/download/v${version}/electron-v${version}-linux-x64.zip";
        sha256 = "0lz6hx91xzp3xwc6d2bhl4290v0ynjfpbnwpm6civ8ijzm6b8zxh";
      };
    }.${stdenv.hostPlatform.system} or throwSystem;
  });
  # closure size TODO list
  # electron depends on cups, which depends on avahi
  daedalus-frontend = writeScriptBin "daedalus-frontend" ''
    #!${stdenv.shell}

    test -z "$XDG_DATA_HOME" && { XDG_DATA_HOME="''${HOME}/.local/share"; }
    export DAEDALUS_DIR="''${XDG_DATA_HOME}/Daedalus"

    cd "''${DAEDALUS_DIR}/${cluster}/"

    exec ${electron_3_0_13}/bin/electron ${rawapp}/share/daedalus "$@"
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
