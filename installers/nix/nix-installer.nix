{ installationSlug ? "nix-install", installedPackages
, postInstall ? null, nix-bundle, preInstall ? null
, linuxClusterBinName
, rawapp
, pkgs }:
let
  installerBundle = nix-bundle.nix-bootstrap {
    target = "${installer}";
    run = "/bin/installer";
    nixUserChrootFlags = "-c -m /home:/home -p HOME";
  };
  utils = pkgs.writeText "utils.sh" ''
    function rmrf {
      chmod -R +w "$*" || true
      rm -rf "$*" || true
    }
  '';
  updater = pkgs.writeScriptBin "update-runner" ''
    #!${pkgs.stdenv.shell}

    source ${utils}

    set -xe

    if [ ! -e "$1" ]; then
      echo "update file not found"
      exit -1
    fi
    pwd
    id
    UNPACK=$(mktemp -d)
    cd $UNPACK
    echo "$@"
    bash "$1" --extract
    ls -ltrh dat/nix/store/*-tarball/tarball/tarball.tar.xz
    UNPACK2=$(mktemp -d)
    tar --delay-directory-restore -C $UNPACK2 -xf dat/nix/store/*-tarball/tarball/tarball.tar.xz
    cd
    rmrf $UNPACK
    ls -ltrh $UNPACK2
    NIX_REMOTE=local?root=$UNPACK2 nix-store --load-db < $UNPACK2/nix-path-registration
    NIX_REMOTE=local?root=$UNPACK2 nix-store --verify --check-contents
    nix copy --no-check-sigs --from local?root=$UNPACK2 $(readlink $UNPACK2/firstGeneration)
    export NIX_PROFILE=/nix/var/nix/profiles/profile
    nix-env --set $(readlink $UNPACK2/firstGeneration)
    nix-env -p /nix/var/nix/profiles/profile-${linuxClusterBinName} --set $(readlink $UNPACK2/firstGeneration)
    rmrf $UNPACK2

    post-install || true
  '';
  enter = pkgs.writeScriptBin "enter-chroot" ''
    #!/usr/bin/env bash

    set -xe

    cd $HOME/${installationSlug}
    mkdir -p etc
    cat /etc/hosts > etc/hosts
    cat /etc/nsswitch.conf > etc/nsswitch.conf
    cat /etc/localtime > etc/localtime
    cat /etc/machine-id > etc/machine-id
    cat /etc/resolv.conf > etc/resolv.conf

    exec .${nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -p TERM -- /nix/var/nix/profiles/profile/bin/enter-phase2
  '';
  fontconf = pkgs.writeText "fonts.conf" ''
    <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
    <fontconfig>
    <dir>${rawapp}</dir>
    </fontconfig>
  '';
  enter2 = pkgs.writeScriptBin "enter-phase2" ''
    #!${pkgs.stdenv.shell}

    set -e

    export PATH=/nix/var/nix/profiles/profile-${linuxClusterBinName}/bin
    export PS1='\[\033]2;\h:\u:\w\007\]\n\[\033[1;32m\][\u@\h:\w] (namespaced) \$\[\033[0m\] '
    ln -svf /nix/var/nix/profiles/profile-${linuxClusterBinName}/bin/ /bin
    export PATH=/bin
    ln -svf ${pkgs.iana-etc}/etc/protocols /etc/protocols
    ln -svf ${pkgs.iana-etc}/etc/services /etc/services
    mkdir -pv /etc/fonts
    ln -svf ${fontconf} /etc/fonts/fonts.conf
    mkdir -pv /etc/ssl/certs
    ln -svf ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt
    ln -svf ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-bundle.crt
    unset NIX_SSL_CERT_FILE

    if [ -z "$@" ]; then
      exec bash
    else
      exec "$@"
    fi
  '';
  installer = with pkgs; writeScriptBin "installer" ''
    #!${stdenv.shell}

    set -e

    TARPATH=${tarball}/tarball/tarball.tar.xz

    if [ ! -z $SOURCING ]; then return; fi

    source ${utils}

    exitHandler() {
      exitCode="$?"
      if [ ! -z $UNPACK ]; then
        rmrf $UNPACK
      fi
      exit "$exitCode"
    }

    trap "exitHandler" EXIT

    export PATH=${lib.makeBinPath [ coreutils pv xz gnutar nix gnused which gnugrep ]}
    export DIR=$HOME/${installationSlug}

    ${if preInstall == null then "" else ''
      source ${preInstall}
    ''}

    echo inside installer
    echo source $TARPATH

    mkdir -pv $HOME/${installationSlug}

    UNPACK=$(mktemp -d)

    pv $TARPATH | unxz | tar --delay-directory-restore -x -C $UNPACK

    NIX_REMOTE=local?root=$UNPACK nix-store --load-db < $UNPACK/nix-path-registration
    pwd
    NIX_REMOTE=local?root=$UNPACK nix-store --verify --check-contents -v
    # turn back on some time later?
    # NIX_REMOTE=local?root=$UNPACK nix copy-sigs --all -s http://cache.nixos.org/

    export NIX_REMOTE=local?root=$DIR
    nix copy --no-check-sigs --from local?root=$UNPACK ${builtins.unsafeDiscardStringContext firstGeneration}
    rmrf $UNPACK
    unset UNPACK
    export NIX_PROFILE=$DIR/nix/var/nix/profiles/profile
    nix-env --set ${builtins.unsafeDiscardStringContext firstGeneration}
    nix-env -p $DIR/nix/var/nix/profiles/profile-${linuxClusterBinName} --set ${builtins.unsafeDiscardStringContext firstGeneration}

    ${if postInstall == null then "" else ''
    exec ${postInstall}/bin/post-install
    ''}
  '';

  firstGeneration = with pkgs; buildEnv {
    name = "profile";
    paths = [
      nix
      bashInteractive
      enter
      coreutils
      enter2
      procps
      updater
      utillinux
      gnused
      gnutar
      bzip2
      gzip
      xz
      which # used by post-install
    ] ++ installedPackages;
  };
  tarball = pkgs.callPackage (pkgs.path + "/nixos/lib/make-system-tarball.nix") {
    fileName = "tarball"; # don't rename
    contents = [];
    storeContents = [
      {
        object = firstGeneration;
        symlink = "firstGeneration";
      }
    ];
  };
in {
  inherit installerBundle pkgs firstGeneration tarball;
}
