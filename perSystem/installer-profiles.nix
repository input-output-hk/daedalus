{...}: {
  perSystem = {
    common,
    pkgs,
    lib,
    ...
  }: let
    bundles = lib.mapAttrs (product: descriptor: let
      configurations =
        lib.genAttrs descriptor.profiles (cluster:
          common.mkDaedalusConfigs {inherit cluster product;});
      backendRevisions = lib.unique (map
        (cluster: (common.walletBackendForCluster cluster).sourceRevision)
        descriptor.profiles);
      manifest = {
        schema = 1;
        inherit product;
        inherit (descriptor) packageName displayName payloadProfile;
        version = common.originalPackageJson.version;
        backendRevision = builtins.head backendRevisions;
        profiles =
          lib.mapAttrs (cluster: configuration: {
            config = "profiles/${cluster}/daedalus-config.json";
            inherit (configuration.daedalusConfig) networkName isFlight;
          })
          configurations;
      };
    in
      assert lib.assertMsg (builtins.length backendRevisions == 1)
      "A shared installer payload requires the same wallet backend for every profile";
        pkgs.runCommand "daedalus-${product}-profile-configs" {
          manifestJson = builtins.toJSON manifest;
          passAsFile = ["manifestJson"];
        } ''
          mkdir -p "$out/profiles"
          cp "$manifestJsonPath" "$out/product.json"
          ${lib.concatMapStringsSep "\n" (cluster: ''
              mkdir -p "$out/profiles/${cluster}"
              cp -r ${configurations.${cluster}.configFiles}/. "$out/profiles/${cluster}/"
            '')
            descriptor.profiles}
        '')
    common.installerProducts;
  in {
    packages = lib.mapAttrs' (product: bundle:
      lib.nameValuePair "${product}-profile-configs" bundle)
    bundles;

    checks.pilot-profile-configs =
      pkgs.runCommand "pilot-profile-configs-check" {
        nativeBuildInputs = [pkgs.jq];
      } ''
          bundle=${bundles.pilot}
          jq -e '.product == "pilot" and .packageName == "daedalus-pilot" and
            (.profiles | keys) == ["mainnet-flight", "preprod", "preview"]' "$bundle/product.json"
          for profile in preview preprod mainnet-flight; do
            config="$bundle/profiles/$profile/daedalus-config.json"
            jq -e --arg profile "$profile" '
              .electron.env.DAEDALUS_IS_FLIGHT == (if $profile == "mainnet-flight" then "true" else "false" end) and
              .electron.env.DAEDALUS_CLUSTER == (if $profile == "mainnet-flight" then "mainnet" else $profile end) and
              .electron.env.DAEDALUS_NETWORK_NAME == (if $profile == "mainnet-flight" then "mainnet" else $profile end)' "$config"
            test -f "$bundle/profiles/$profile/config.yaml"
            test -f "$bundle/profiles/$profile/genesis-byron.json"
          done
          test ! -d "$bundle/profiles/mainnet"
          ${lib.concatMapStringsSep "\n" (profile: ''
            jq -n -e \
              --slurpfile selected "$bundle/profiles/${profile}/daedalus-config.json" \
              --slurpfile prior ${common.daedalusConfigs.${profile}.configFiles}/daedalus-config.json \
              '$selected[0].node.state_dir == $prior[0].node.state_dir and
               $selected[0].wallet.state_dir == $prior[0].wallet.state_dir and
               $selected[0].node.socket_path == $prior[0].node.socket_path'
          '')
          common.installerProducts.pilot.profiles}
        ${lib.concatMapStringsSep "\n" (profile: let
            legacy = common.daedalusConfigs.${profile};
            settingsRoot =
              if pkgs.stdenv.hostPlatform.isLinux
              then "\${XDG_CONFIG_HOME}/${legacy.installerConfig.spacedName}"
              else legacy.installerConfig.dataDir;
          in ''
            jq -e --arg argument ${lib.escapeShellArg ("--user-data-dir=" + settingsRoot)} \
              '.electron.args == [$argument]' "$bundle/profiles/${profile}/daedalus-config.json"
          '')
          common.installerProducts.pilot.profiles}
        # Each selected environment must resolve its own node configuration inside
        # the shared installation root, while standalone Mainnet keeps its layout.
        ${lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
          for profile in preview preprod mainnet-flight; do
            jq -e --arg path "\''${ENTRYPOINT_DIR}/config/profiles/$profile/config.yaml" \
              '.node.args[2] == $path' "$bundle/profiles/$profile/daedalus-config.json"
          done
          jq -e '.node.args[2] == "''${ENTRYPOINT_DIR}/config/config.yaml"' \
            ${common.daedalusConfigs.mainnet.configFiles}/daedalus-config.json
        ''}
          touch "$out"
      '';
  };
}
