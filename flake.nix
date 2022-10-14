{
  description = "Cicero jobs populating https://cache.iog.io – this cannot yet build Daedalus";
  inputs = {};
  outputs = inputs: {
    hydraJobs = {

      # --- Linux ----------------------------------------------------
      x86_64-linux = let
        d = import ./default.nix { target = "x86_64-linux"; localLibSystem = "x86_64-linux"; };
      in {
        cardano-bridge = d.daedalus-bridge;
        cardano-node = d.cardano-node;
        # daedalus = d.daedalus;  # TODO: I’m really not sure if it still makes sense, if we have Buildkite…
        daedalus-installer = d.daedalus-installer;
        devShellGCRoot = (import ./shell.nix { system = "x86_64-linux"; autoStartBackend = true; }).gcRoot;
        mono = d.pkgs.mono;
        nodejs = d.nodejs;
        tests = d.tests;
        wine = d.wine;
        wine64 = d.wine64;
        yaml2json = d.yaml2json;
      };
      # --------------------------------------------------------------

      # --- Windows (x-compiled from Linux) --------------------------
      x86_64-windows = let
        d = import ./default.nix { target = "x86_64-windows"; localLibSystem = "x86_64-linux"; };
      in {
        cardano-bridge = d.daedalus-bridge;
        cardano-node = d.cardano-node;
      };
      # --------------------------------------------------------------

      # --- Darwin ---------------------------------------------------
      x86_64-darwin = let
        d = import ./default.nix { target = "x86_64-darwin"; localLibSystem = "x86_64-darwin"; };
      in {
        cardano-bridge = d.daedalus-bridge;
        cardano-node = d.cardano-node;
        daedalus-installer = d.daedalus-installer;
        devShellGCRoot = (import ./shell.nix { system = "x86_64-darwin"; autoStartBackend = true; }).gcRoot;
        nodejs = d.nodejs;
        yaml2json = d.yaml2json;
      };
      # --------------------------------------------------------------

    };
  };
  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = ["https://cache.iog.io" "https://iog.cachix.org"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "iog.cachix.org-1:nYO0M9xTk/s5t1Bs9asZ/Sww/1Kt/hRhkLP0Hhv/ctY="];
    allow-import-from-derivation = "true";
  };
  # --------------------------------------------------------------
}
