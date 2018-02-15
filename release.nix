let
  pkgs = import (import ./fetchNixpkgs.nix {
    rev = "c831224528cd6bfd49bfc2c18b9c5d9015651077";
    sha256 = "1idygi6x10wilrmj4w1djby8qi8zphxbp1zkzn4lmwhzfhgx2qnz";
  }) {
    config = {};
    overlays = [];
  };
  packages = self: {
    daedalus_internal = self.callPackage ./linux.nix {};
    daedalus = self.daedalus_internal.daedalus;
    tarball = self.callPackage ./tarball.nix {};
    electron = pkgs.enableDebugging pkgs.electron;
    static_patchelf = (pkgs.patchelf.override { stdenv = pkgs.makeStaticBinaries pkgs.stdenv; }).overrideAttrs (drv: {
      buildInputs = [ pkgs.glibc.static ];
    });
  };
in pkgs.lib.makeScope pkgs.newScope packages
