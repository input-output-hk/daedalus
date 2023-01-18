{ pkgs }:

with pkgs.haskell.lib;

self: super: {
  daedalus-installer = self.callPackage ../daedalus-installer.nix {};
  universum = dontCheck (self.callPackage ./universum.nix {});
  nsis = self.callPackage ./nsis.nix {};
  github = self.callPackage ./github.nix {};
}
