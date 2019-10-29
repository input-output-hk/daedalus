{ nodeImplementation ? "jormungandr" }:

let
  sources = import ./nix/sources.nix;
  iohkNix = import sources.iohk-nix { sourcesOverride = sources; };
  # TODO: can we use the filter in iohk-nix instead?
  cleanSourceFilter = with pkgs.stdenv;
    name: type: let baseName = baseNameOf (toString name); in ! (
      # Filter out .git repo
      (type == "directory" && baseName == ".git") ||
      # Filter out editor backup / swap files.
      lib.hasSuffix "~" baseName ||
      builtins.match "^\\.sw[a-z]$" baseName != null ||
      builtins.match "^\\..*\\.sw[a-z]$" baseName != null ||

      # Filter out locally generated/downloaded things.
      baseName == "dist" ||
      baseName == "node_modules" ||

      # Filter out the files which I'm editing often.
      lib.hasSuffix ".nix" baseName ||
      lib.hasSuffix ".dhall" baseName ||
      lib.hasSuffix ".hs" baseName ||
      # Filter out nix-build result symlinks
      (type == "symlink" && lib.hasPrefix "result" baseName)
    );
  rustPkgs = iohkNix.rust-packages.pkgs;
  isDaedalus = name: false;
  cardanoSL = { target }: import sources.cardano-sl { gitrev = sources.cardano-sl.rev; };
  cardanoWallet' = import sources.cardano-wallet {};
  cardanoWallet = if nodeImplementation == "jormungandr" then cardanoWallet'.cardano-wallet-jormungandr else cardanoWallet'.cardano-wallet-http-bridge;
  cardanoNode = if nodeImplementation == "jormungandr" then cardanoWallet'.jormungandr else cardanoWallet'.cardano-http-bridge;
  jormungandr = rustPkgs.jormungandr;
  jcli = cardanoWallet'.jormungandr-cli;
  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
in
lib // {
  inherit sources iohkNix pkgs cardanoSL isDaedalus cleanSourceFilter cardanoWallet cardanoNode jcli;
  inherit (rustPkgs) jormungandr;
}
