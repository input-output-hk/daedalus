let
  # iohk-nix can be overridden for debugging purposes by setting
  # NIX_PATH=iohk_nix=/path/to/iohk-nix
  iohkNix = import (
    let try = builtins.tryEval <iohk_nix>;
    in if try.success
    then builtins.trace "using host <iohk_nix>" try.value
    else
      let
        spec = builtins.fromJSON (builtins.readFile ./iohk-nix.json);
      in builtins.fetchTarball {
        url = "${spec.url}/archive/${spec.rev}.tar.gz";
        inherit (spec) sha256;
      }) {};

  # NIX_PATH=cardano-sl=/path/to/cardano-sl
  # WARNING: currently broken with infinite recursion
  cardanoSL = { config ? {}, system ? builtins.currentSystem }:
    let try = builtins.tryEval <cardano-sl>;
    in if try.success
    then builtins.trace "using host <cardano-sl>" (import try.value { inherit system config; })
    else
      let
        spec = builtins.fromJSON (builtins.readFile ./cardano-sl-src.json);
      in import (builtins.fetchTarball {
        url = "${spec.url}/archive/${spec.rev}.tar.gz";
        inherit (spec) sha256;
      }) { inherit system config; gitrev = spec.rev; };

  # nixpkgs can be overridden for debugging purposes by setting
  # NIX_PATH=custom_nixpkgs=/path/to/nixpkgs
  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
  isDaedalus = name: false;
  # TODO: Update once this branch is merged into input-output-hk/cardano-wallet
  cardanoWalletSrc = import (pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-wallet";
    rev = "0129cb2f0a40d9801c13ec64058899810c847509";
    sha256 = "0kic2mbkjyp433xmi8203j62qfw34yfkxdpalb3qcqh2652s5371";
  }) {};
  cardanoWallet = cardanoWalletSrc.cardano-wallet;
  cardanoNode = cardanoWalletSrc.cardano-http-bridge;
in lib // {
  inherit iohkNix pkgs cardanoSL isDaedalus cardanoWallet cardanoNode;
}
