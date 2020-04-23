{ pkgs }:

with pkgs.haskell.lib;

self: super: {
  daedalus-installer = self.callPackage ../daedalus-installer.nix {};
  dhall-json = self.callPackage ./dhall-json.nix {};
  dhall = dontCheck (doJailbreak (self.callPackage ./dhall.nix {}));
  universum = dontCheck (self.callPackage ./universum.nix {});
  nsis = self.callPackage ./nsis.nix {};
}
