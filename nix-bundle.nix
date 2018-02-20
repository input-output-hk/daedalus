{ pkgs, daedalus, nix-bundle, coreutils, utillinux, procps }:

let
  bundle = import (pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "nix-bundle";
    rev = "630e89d1d16083";
    sha256 = "1s9vzlsfxd2ym8jzv2p64j6jlwr9cmir45mb12yzzjr4dc91xk8x";
  }) { nixpkgs = pkgs; };
  wrapper = pkgs.writeScriptBin "daedalus-wrapper" ''
  #!${pkgs.stdenv.shell}

  export PATH=${coreutils}/bin:${utillinux}/bin:${procps}/bin

  mkdir /etc/
  cp /host-etc/machine-id /etc/machine-id
  cp /host-etc/resolv.conf /etc/resolv.conf
  ln -sv ${pkgs.iana-etc}/etc/protocols /etc/protocols
  ln -sv ${pkgs.iana-etc}/etc/services /etc/services

  exec ${daedalus}/bin/daedalus
  '';
  foo = bundle.nix-bootstrap {
    target = "${wrapper}";
    run = "/bin/daedalus-wrapper";
    nixUserChrootFlags = "-c -m /home:/home -m /etc:/host-etc -p DISPLAY -p HOME -p XAUTHORITY";
  };
in foo
