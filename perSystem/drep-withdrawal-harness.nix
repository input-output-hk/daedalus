# Retired-DRep reward withdrawal harness (tests/cluster/drep-withdrawal), run
# against the cardano-node, cardano-cli, cardano-address and both
# cardano-wallet builds this flake pins:
#
#   nix run .#drep-withdrawal-harness -- --help
#
# The mainnet build's wallet is the `cardano-wallet` input and the
# mainnet-flight, preprod and preview builds' wallet is `cardano-wallet-cip30`,
# as perSystem/common.nix maps them. Another wallet is tested in either role
# by overriding that input:
#
#   nix run .#drep-withdrawal-harness \
#     --override-input cardano-wallet github:<owner>/cardano-wallet/<ref> -- --backend upstream
{...}: {
  perSystem = {
    pkgs,
    lib,
    system,
    common,
    ...
  }:
    lib.mkIf (system == "x86_64-linux") (let
      inherit (common) walletBackends;
      harness = pkgs.writeShellApplication {
        name = "drep-withdrawal-harness";
        runtimeInputs = with pkgs; [coreutils curl gawk gnugrep gnused iproute2 jq sqlite xxd];
        text = ''
          export HARNESS_NODE=${lib.getExe' common.cardano-node "cardano-node"}
          export HARNESS_CLI=${lib.getExe' common.cardano-cli "cardano-cli"}
          export HARNESS_ADDRESS=${lib.getExe' common.cardano-address "cardano-address"}
          export HARNESS_WALLET_UPSTREAM=${lib.getExe' walletBackends.upstream.packages.cardano-wallet "cardano-wallet"}
          export HARNESS_WALLET_CIP30=${lib.getExe' walletBackends.cip30.packages.cardano-wallet "cardano-wallet"}
          exec ${pkgs.bash}/bin/bash ${../tests/cluster/drep-withdrawal}/harness.sh "$@"
        '';
      };
    in {
      apps.drep-withdrawal-harness = {
        type = "app";
        program = lib.getExe harness;
      };
    });
}
