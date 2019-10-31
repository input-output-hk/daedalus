{ pkgs }:

with import ../../lib.nix;

with pkgs.haskell.lib;

self: super: {
      dhall-json = self.callHackageDirect { pkg = "dhall-json"; ver = "1.4.1"; sha256 = "0b20p09vz6z8a8iyf832xscjvkcc68rrhr54pwdsbzikp9mdxx74"; } {};
      dhall = doJailbreak (self.callHackageDirect { pkg = "dhall"; ver = "1.26.1"; sha256 = "12js4mgrmy4vral00x3a0w06rpgz0yy1p45k2xv64gg7gx8qp46y"; } {});

      #dhall = self.callPackage ../../installers/dhall-haskell.nix {});
      daedalus-installer = self.callPackage ../../installers/daedalus-installer.nix {};
      universum = dontCheck (self.callHackage "universum" "1.5.0" {});
      nsis = self.callCabal2nix "nsis" (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "haskell-nsis";
        rev = "020e61eced93eaa6ab86ac603617e93aa6bf5af0";
        sha256 = "0l0naknnyyrmkrn41mn7ggnjdagld0isdji98khn2iasbcllyip5";
      }) {};
}
