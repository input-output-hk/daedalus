{ inputs, buildSystem }:

let

  inherit (inputs.nixpkgs) lib;

  mkPackages = targetSystem: let
    internal = inputs.self.internal.${targetSystem}; # don’t eval again
    suffix = if buildSystem != targetSystem then "-${targetSystem}" else "";
  in (lib.listToAttrs (lib.concatMap (cluster: [
    {
      name = "daedalus-${cluster}${suffix}";
      value = internal.${cluster}.package;
    }
    {
      name = "installer-${cluster}${suffix}";
      value = internal.${cluster}.unsignedInstaller;
    }
    {
      name = "makeSignedInstaller-${cluster}${suffix}";
      value = internal.${cluster}.makeSignedInstaller;
    }
  ]) inputs.self.internal.installerClusters)) // {
    default = internal.mainnet.package;
    "buildkitePipeline${suffix}" = import ./internal/buildkite-pipeline.nix { inherit inputs targetSystem; };
  };

in {
  x86_64-linux = mkPackages "x86_64-windows" // mkPackages "x86_64-linux";
  x86_64-darwin = mkPackages "x86_64-darwin";
  aarch64-darwin = mkPackages "aarch64-darwin";
}.${buildSystem}
