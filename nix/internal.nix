{ inputs }:

let
  inherit (inputs.nixpkgs) lib;
  inherit (import ./internal/source-lib.nix { inherit inputs; }) installerClusters;
in

(__mapAttrs (targetSystem: src: import src { inherit inputs targetSystem; }) {
  x86_64-linux = ./internal/x86_64-linux.nix;
  x86_64-windows = ./internal/x86_64-windows.nix;
  x86_64-darwin = ./internal/any-darwin.nix;
  aarch64-darwin = ./internal/any-darwin.nix;
}) // { inherit installerClusters; }
