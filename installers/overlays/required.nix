{ pkgs }:

with import ../../lib.nix;

with pkgs.haskell.lib;

self: super: rec {
      dhall-json = doJailbreak (self.callPackage ../../installers/dhall-json.nix {});
      dhall = doJailbreak (self.callPackage ../../installers/dhall-haskell.nix {});
      daedalus-installer = self.callPackage ../../installers/daedalus-installer.nix {};
}
