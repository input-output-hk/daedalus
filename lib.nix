{ nodeImplementation }:

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
  cardanoSL = { config ? {}, target }:
    let try = builtins.tryEval <cardano-sl>;
    in if try.success
    then builtins.trace "using host <cardano-sl>" (import try.value { inherit target; })
    else
      let
        spec = builtins.fromJSON (builtins.readFile ./cardano-sl-src.json);
      in import (builtins.fetchTarball {
        url = "${spec.url}/archive/${spec.rev}.tar.gz";
        inherit (spec) sha256;
      }) { inherit target; gitrev = spec.rev; };

  # nixpkgs can be overridden for debugging purposes by setting
  # NIX_PATH=custom_nixpkgs=/path/to/nixpkgs
  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
  isDaedalus = name: false;
  cardanoWalletSrc = import (pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-wallet";
    rev = "7208a54a018415108dc359b7e20cb024bef1af63";
    sha256 = "0nqvf9m3kv83vb1dgbf0n6nf1iwik9wq91hmhy5q1dhzcdxj2ffs";
  }) {};

  cardanoWallet = if nodeImplementation == "jormungandr" then cardanoWalletSrc.cardano-wallet-jormungandr else cardanoWalletSrc.cardano-wallet-http-bridge;
  cardanoNode = if nodeImplementation == "jormungandr" then cardanoWalletSrc.jormungandr else cardanoWalletSrc.cardano-http-bridge;
  jcli = cardanoWalletSrc.jormungandr-cli;
in lib // {
  inherit iohkNix pkgs cardanoSL isDaedalus cardanoWallet cardanoNode jcli;
}
