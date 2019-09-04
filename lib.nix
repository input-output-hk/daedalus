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
  # inherit (iohkNix.rust-packages.pkgs) jormungandr;
  cardanoWalletSrc = import (pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-wallet";
    rev = "b9a860ff3590cf80cc6aea519ecac0be44c62c35";
    sha256 = "11j3an4i6kvk9h71hz1cngns2a47cmv38bsr3qyagcr1829rlrxl";
  }) {};
  cardanoWallet = cardanoWalletSrc.cardano-wallet-jormungandr;
  cardanoNode = cardanoWalletSrc.jormungandr;
in lib // {
  inherit iohkNix pkgs cardanoSL isDaedalus cardanoWallet cardanoNode;
}
