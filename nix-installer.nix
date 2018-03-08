{ installationSlug ? "nix-install", installedPackages }:
let
  pkgs = import (import ./fetchNixpkgs.nix (builtins.fromJSON (builtins.readFile ./nixpkgs-src.json))) { config = {}; overlays = []; };
  nix-bundle = import (pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "nix-bundle";
    rev = "630e89d1d16083";
    sha256 = "1s9vzlsfxd2ym8jzv2p64j6jlwr9cmir45mb12yzzjr4dc91xk8x";
  }) { nixpkgs = pkgs; };
  installerBundle = nix-bundle.nix-bootstrap {
    target = "${installer}";
    run = "/bin/installer";
    nixUserChrootFlags = "-c -m /home:/home -p HOME";
  };
  nixSrc = pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nix";
    rev = "16551f54c94f2b551ebaf00a7bd0245dc3b0b9e4";
    sha256 = "0kd13v4xl4imwb3141pnn0lqx0xfcmgnwd026c10kmvjhm848pwx";
  };
  nixFix = (import ./nix/release.nix {
    nixpkgs = pkgs.path;
  }).build.x86_64-linux;
  utils = pkgs.writeText "utils.sh" ''
    function rmrf {
      chmod -R +w "$*" || true
      rm -rf "$*" || true
    }
  '';
  updater = pkgs.writeScriptBin "update-runner" ''
    #!${pkgs.stdenv.shell}

    source ${utils}

    set -x
    set -e
    pwd
    id
    UNPACK=$(mktemp -d)
    cd $UNPACK
    echo "$@"
    "$1" --extract
    ls -ltrh dat/nix/store/*-tarball/tarball/tarball.tar.xz
    UNPACK2=$(mktemp -d)
    tar -C $UNPACK2 -xf dat/nix/store/*-tarball/tarball/tarball.tar.xz
    cd
    rmrf $UNPACK
    ls -ltrh $UNPACK2
    NIX_REMOTE=local?root=$UNPACK2 nix-store --load-db < $UNPACK2/nix-path-registration
    NIX_REMOTE=local?root=$UNPACK2 nix-store --verify --check-contents
    nix copy --no-check-sigs --from local?root=$UNPACK2 $(readlink $UNPACK2/firstGeneration)
    export NIX_PROFILE=$DIR/nix/var/nix/profiles/profile
    nix-env --set $(readlink $UNPACK2/firstGeneration)
    rmrf $UNPACK2
  '';
  enter = pkgs.writeScriptBin "enter-chroot" ''
    #!/usr/bin/env bash

    set -x
    set -e

    cd $HOME/${installationSlug}
    mkdir -p etc
    cat /etc/hosts > etc/hosts
    cat /etc/nsswitch.conf > etc/nsswitch.conf
    cat /etc/machine-id > etc/machine-id
    cat /etc/resolv.conf > etc/resolv.conf

    exec .${nix-bundle.nix-user-chroot}/bin/nix-user-chroot -n ./nix -c -m /home:/home -m /etc:/host-etc -m etc:/etc -p DISPLAY -p HOME -p XAUTHORITY -- /nix/var/nix/profiles/profile/bin/enter-phase2
  '';
  enter2 = pkgs.writeScriptBin "enter-phase2" ''
    #!${pkgs.stdenv.shell}

    export PATH=/nix/var/nix/profiles/profile/bin
    export PS1='\[\033]2;\h:\u:\w\007\]\n\[\033[1;32m\][\u@\h:\w] (namespaced) \$\[\033[0m\] '
    ln -svf /nix/var/nix/profiles/profile/bin/ /bin
    ln -svf ${pkgs.iana-etc}/etc/protocols /etc/protocols
    ln -svf ${pkgs.iana-etc}/etc/services /etc/services
    exec bash
  '';
  installer = with pkgs; writeScriptBin "installer" ''
    #!${stdenv.shell}
    TARPATH=${tarball}/tarball/tarball.tar.xz

    if [ -z $SOURCING ]; then return; fi

    source ${utils}

    set -e
    set -x

    export PATH=${lib.makeBinPath [ coreutils pv xz gnutar nixFix strace ]}
    export DIR=$HOME/${installationSlug}

    echo inside installer
    echo source $TARPATH

    mkdir -pv $HOME/nix-install

    UNPACK=$(mktemp -d)

    pv $TARPATH | unxz | tar -x -C $UNPACK

    NIX_REMOTE=local?root=$UNPACK nix-store --load-db < $UNPACK/nix-path-registration
    pwd
    NIX_REMOTE=local?root=$UNPACK nix-store --verify --check-contents -v
    NIX_REMOTE=local?root=$UNPACK nix copy-sigs --all -s http://cache.nixos.org/

    export NIX_REMOTE=local?root=$DIR
    nix copy --no-check-sigs --from local?root=$UNPACK ${builtins.unsafeDiscardStringContext firstGeneration}
    rmrf $UNPACK
    export NIX_PROFILE=$DIR/nix/var/nix/profiles/profile
    nix-env --set ${builtins.unsafeDiscardStringContext firstGeneration}
  '';

  firstGeneration = with pkgs; buildEnv {
    name = "profile";
    paths = [ nixFix bashInteractive enter coreutils enter2 procps updater utillinux gnused gnutar bzip2 xz ] ++ installedPackages;
  };
  tarball = pkgs.callPackage (pkgs.path + "/nixos/lib/make-system-tarball.nix") {
    fileName = "tarball";
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
