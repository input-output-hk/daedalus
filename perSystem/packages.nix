{inputs, ...}: {
  perSystem = {
    system,
    lib,
    pkgs,
    ...
  }: let
    # For cross-compilation support, we need to determine the build and target systems
    # x86_64-linux can build for both x86_64-linux and x86_64-windows
    mkPackages = targetSystem: let
      internal = inputs.self.internal.${targetSystem};
      suffix =
        if system != targetSystem
        then "-${targetSystem}"
        else "";
    in
      (lib.listToAttrs (lib.concatMap (cluster: [
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
          {
            name = "daedalus-bridge-${cluster}${suffix}";
            value = internal.common.daedalus-bridge.${cluster};
          }
        ])
        inputs.self.internal.installerClusters))
      // {
        "buildkitePipeline${suffix}" = import ../nix/internal/buildkite-pipeline.nix {
          inherit inputs;
          targetSystem = targetSystem;
        };
      };
  in {
    packages =
      if system == "x86_64-linux"
      then mkPackages "x86_64-windows" // mkPackages "x86_64-linux" // {default = inputs.self.internal.x86_64-linux.package.mainnet;}
      else if system == "x86_64-darwin"
      then mkPackages "x86_64-darwin" // {default = inputs.self.internal.x86_64-darwin.package.mainnet;}
      else if system == "aarch64-darwin"
      then mkPackages "aarch64-darwin" // {default = inputs.self.internal.aarch64-darwin.package.mainnet;}
      else {};
  };
}
