{inputs, ...}: {
  perSystem = {
    system,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (system == "x86_64-linux") (let
      internal = inputs.self.internal.x86_64-linux;
      inherit (internal) common originalPackageJson newPackage;
      inherit (common) sourceLib launcherConfigs;
      installerClusters = inputs.self.internal.installerClusters;
      genClusters = lib.genAttrs installerClusters;

      commonPackagingArgs = cluster: {
        inherit pkgs lib cluster;
        bundle = systemPackageBundle.${cluster};
        icon = launcherConfigs.${cluster}.installerConfig.iconPath.base + "/512x512.png";
        version = originalPackageJson.version;
        inherit (sourceLib) buildCounter buildRev buildRevShort;
        sourceDateEpoch = inputs.self.lastModified or sourceLib.daedalusEpoch;
      };

      systemPackageBundle = genClusters (cluster:
        pkgs.stdenv.mkDerivation {
          name = "daedalus-system-package-bundle";
          dontUnpack = true;
          buildCommand = ''
            cp -r ${newPackage.${cluster}} $out
            chmod -R +w $out
            for symlink in $out/libexec/{daedalus-js,bundle-*} ; do
              target=$(readlink "$symlink")
              rm "$symlink"
              cp -r "$target" "$symlink"
            done
            find $out/libexec/daedalus-js/ -type f -iname '*.node' | while IFS= read -r file ; do
              chmod +w "$file"
              patchelf --set-rpath \
                "\$ORIGIN/$(realpath --relative-to="$(dirname "$file")" $out/libexec/bundle-electron/lib/electron/lib)" \
                "$file"
            done
            rm -rf $out/share/applications
            rm -f $out/share/icon_large.png
            rm -f $out/libexec/update-runner
          '';
        });

      debInstaller = genClusters (cluster:
        import ../../packaging/linux/deb.nix (commonPackagingArgs cluster));
    in {
      packages =
        lib.listToAttrs (lib.concatMap (cluster: [
          {
            name = "deb-installer-${cluster}";
            value = debInstaller.${cluster};
          }
        ])
        installerClusters);
    });
}
