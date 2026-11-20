{inputs, ...}: {
  perSystem = {
    system,
    inputs',
    pkgs,
    lib,
    common,
    ...
  }:
    lib.optionalAttrs (system == "x86_64-linux") (let
      toolchain = with inputs'.fenix.packages;
        combine [stable.rustc stable.cargo];
      craneLib = (inputs.crane.mkLib pkgs).overrideToolchain toolchain;
      buildArgs = {
        src = craneLib.cleanCargoSource inputs.quetzal;
        pname = "quetzal";
        version = "0.1.0-${builtins.substring 0 9 inputs.quetzal.rev}";
        strictDeps = true;
        cargoExtraArgs = "--locked --package quetzal-api --bin quetzal";
      };
      cargoArtifacts = craneLib.buildDepsOnly buildArgs;
      quetzal = craneLib.buildPackage (buildArgs
        // {
          inherit cargoArtifacts;
          # Upstream workspace tests are separate from this integration experiment.
          doCheck = false;
          meta = {
            description = "Quetzal wallet backend for the Daedalus development experiment";
            mainProgram = "quetzal";
            license = lib.licenses.asl20;
            platforms = ["x86_64-linux"];
          };
        });
      experimental = cluster:
        common.mkDaedalusConfigs {
          inherit cluster;
          devShell = true;
          quetzal = true;
        };
      reference = cluster:
        common.mkDaedalusConfigs {
          inherit cluster;
          devShell = true;
        };
      packaged = cluster:
        common.mkDaedalusConfigs {
          inherit cluster;
          quetzal = true;
        };
      checkNetwork = cluster: let
        candidate = experimental cluster;
        original = reference cluster;
      in ''
        candidate=${candidate.configFiles}/daedalus-config.json
        reference=${original.configFiles}/daedalus-config.json
        jq -e --arg revision '${inputs.quetzal.rev}' --arg cluster '${cluster}' '
          .wallet.exe == "${quetzal}/bin/quetzal" and
          .wallet.state_dir == ("\u0024{DAEDALUS_DEV_ROOT}/state-quetzal-" + $cluster) and
          .node.state_dir == .wallet.state_dir and
          .tls_dir == (.wallet.state_dir + "/tls") and
          .pub_logs_dir == (.wallet.state_dir + "/Logs/pub") and
          .electron.env.DAEDALUS_STATE_DIR == .wallet.state_dir and
          .electron.env.DAEDALUS_LOGS_DIR == (.wallet.state_dir + "/Logs") and
          (.electron.args | index("--user-data-dir=\u0024{DAEDALUS_DEV_ROOT}/state-quetzal-" + $cluster + "/frontend")) != null and
          .wallet.args[0] == "serve" and
          (.wallet.args | .[index("--database") + 1]) == (.wallet.state_dir + "/wallets/quetzal.sled") and
          (.wallet.args | index("--testnet")) != null and
          (.wallet.args | index("--sync-tolerance")) != null and
          (.wallet.args | index("--token-metadata-server")) != null and
          .wallet.health == {} and
          .cardanoWalletSourceRevision == $revision and
          .cip30Enabled == false and
          .dappBrowserPolicy.globalEnabled == false and
          .dappBrowserPolicy.hardwareConnectorEnabled == false
        ' "$candidate" > /dev/null
        jq -e --arg cluster '${cluster}' '
          .wallet.exe == "cardano-wallet" and
          .wallet.state_dir == ("\u0024{DAEDALUS_DEV_ROOT}/state-" + $cluster) and
          (.wallet.args | index("--sync-tolerance")) != null and
          .cip30Enabled == true
        ' "$reference" > /dev/null
        test ! -e ${candidate.configFiles}/shelley-genesis.json
        for config in "$candidate" "$reference"; do
          genesis=$(jq -r '.wallet.args | .[index("--testnet") + 1]' "$config")
          test -f "$genesis"
          jq -e '.protocolConsts.protocolMagic == ${
          if cluster == "preview"
          then "2"
          else "1"
        }' "$genesis" > /dev/null
        done
      '';
    in {
      packages = {
        inherit quetzal;
        quetzal-preview-config = (experimental "preview").configFiles;
        quetzal-preprod-config = (experimental "preprod").configFiles;
        quetzal-preview-package-config = (packaged "preview").configFiles;
        quetzal-preprod-package-config = (packaged "preprod").configFiles;
      };
      checks.quetzal-config-contract =
        pkgs.runCommand "quetzal-config-contract" {
          nativeBuildInputs = [pkgs.jq];
        } ''
          ${checkNetwork "preview"}
          ${checkNetwork "preprod"}
          ${lib.concatMapStringsSep "\n" (cluster: ''
            jq -e --arg cluster '${cluster}' --arg revision '${inputs.quetzal.rev}' '
              .wallet.exe == "\u0024{ENTRYPOINT_DIR}/libexec/quetzal" and
              .wallet.state_dir == ("\u0024{DAEDALUS_DIR}/" + $cluster) and
              .node.state_dir == .wallet.state_dir and
              .electron.env.DAEDALUS_STATE_DIR == .wallet.state_dir and
              .electron.env.DAEDALUS_LOGS_DIR == (.wallet.state_dir + "/Logs") and
              .electron.args == [] and
              (.wallet.args | .[index("--database") + 1]) == (.wallet.state_dir + "/wallets/quetzal.sled") and
              .cardanoWalletSourceRevision == $revision and
              .cip30Enabled == false and
              .dappBrowserPolicy.globalEnabled == false
            ' ${(packaged cluster).configFiles}/daedalus-config.json > /dev/null
          '') ["preview" "preprod"]}
          touch "$out"
        '';
    });
}
