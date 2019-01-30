{ pkgs }:

with import ../../lib.nix;

with pkgs.haskell.lib;

self: super: rec {
      dhall-json = doJailbreak (self.callPackage ../../installers/dhall-json.nix {});
      dhall = doJailbreak (self.callPackage ../../installers/dhall-haskell.nix {});
      daedalus-installer = self.callPackage ../../installers/daedalus-installer.nix {};
      nsis = self.callCabal2nix "nsis" (pkgs.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "haskell-nsis";
        rev = "020e61eced93eaa6ab86ac603617e93aa6bf5af0";
        sha256 = "0l0naknnyyrmkrn41mn7ggnjdagld0isdji98khn2iasbcllyip5";
      }) {};
}
