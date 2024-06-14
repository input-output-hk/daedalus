{ inputs, buildSystem }:

let

  inherit (inputs.nixpkgs) lib;

  mkPackages = targetSystem: let
    internal = inputs.self.internal.${targetSystem}; # don’t eval again
    suffix = if buildSystem != targetSystem then "-${targetSystem}" else "";
  in (lib.listToAttrs (lib.concatMap (cluster: [
    {
      name = "daedalus-${cluster}${suffix}";
      value = internal.package.${cluster};
    }
    {
      name = "installer-${cluster}${suffix}";
      value = internal.unsignedInstaller.${cluster};
    }
    {
      name = "makeSignedInstaller-${cluster}${suffix}";
      value = internal.makeSignedInstaller.${cluster};
    }
  ]) inputs.self.internal.installerClusters)) // {
    default = internal.package.mainnet;
    "buildkitePipeline${suffix}" = import ./internal/buildkite-pipeline.nix { inherit inputs targetSystem; };
  };

in {
  x86_64-linux = mkPackages "x86_64-windows" // mkPackages "x86_64-linux";
  x86_64-darwin = mkPackages "x86_64-darwin";
  aarch64-darwin = mkPackages "aarch64-darwin";
}.${buildSystem}
