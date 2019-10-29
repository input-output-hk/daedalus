{ system ? builtins.currentSystem
, config ? {}
, haskellCompiler ? "ghc865"
}:

with import ../lib.nix {};

let
  haskellCompiler = "ghc865";
  haskellNixOverlay = import (sources."haskell.nix" + "/overlays");
  pkgs = import sources.nixpkgs { overlays = haskellNixOverlay; inherit system config; };

in with pkgs.haskell-nix; cabalProject {
  src = ./.;
  ghc = pkgs.buildPackages.pkgs.haskell.compiler.${haskellCompiler};
}
