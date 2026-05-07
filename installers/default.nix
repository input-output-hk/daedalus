{
  system ? builtins.currentSystem,
  config ? {},
  pkgs,
}: let
  haskellPackages = pkgs.haskell.packages.ghc967.override {
    overrides = import ./overlays/required.nix {inherit pkgs;};
  };
  #haskellPackages = haskellPackages1.extend (import ./overlays/add-test-stubs.nix { inherit pkgs daedalus-bridge; });
in
  haskellPackages // {inherit pkgs;}
