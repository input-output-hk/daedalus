{inputs, ...}: {
  perSystem = {
    system,
    pkgs,
    lib,
    darwinBuild,
    ...
  }:
    lib.mkIf (system == "x86_64-darwin" || system == "aarch64-darwin") (let
      clusters = darwinBuild.common.sourceLib.installerClusters;
    in {
      packages =
        lib.listToAttrs (lib.concatMap (cluster: [
            {
              name = "daedalus-${cluster}";
              value = darwinBuild.package.${cluster};
            }
            {
              name = "installer-${cluster}";
              value = darwinBuild.unsignedInstaller.${cluster};
            }
            {
              name = "makeSignedInstaller-${cluster}";
              value = darwinBuild.makeSignedInstaller.${cluster};
            }
            {
              name = "daedalus-bridge-${cluster}";
              value = darwinBuild.common.daedalus-bridge.${cluster};
            }
          ])
          clusters)
        // {default = darwinBuild.package.mainnet;};
    });
}
