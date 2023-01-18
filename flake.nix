{
  description = "Cicero jobs populating https://cache.iog.io – this cannot yet build Daedalus";
  inputs = {
    nixpkgs.follows = "tullia/nixpkgs";
    tullia.url = "github:input-output-hk/tullia";
  };
  outputs = inputs: {
    hydraJobs = rec {

      # --- Linux ----------------------------------------------------
      x86_64-linux.x86_64-linux = let
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
      x86_64-linux.x86_64-windows = let
        d = import ./default.nix { target = "x86_64-windows"; localLibSystem = "x86_64-linux"; };
      in {
        cardano-bridge = d.daedalus-bridge;
        cardano-node = d.cardano-node;
      };
      # --------------------------------------------------------------

      # --- Darwin ---------------------------------------------------
      x86_64-darwin.x86_64-darwin = let
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

      # --- What CI should build -------------------------------------
      x86_64-linux.required = inputs.nixpkgs.legacyPackages.x86_64-linux.releaseTools.aggregate {
        name = "required for CI";
        constituents = __attrValues x86_64-linux.x86_64-linux ++ __attrValues x86_64-linux.x86_64-windows;
      };
      x86_64-darwin.required = inputs.nixpkgs.legacyPackages.x86_64-darwin.releaseTools.aggregate {
        name = "required for CI";
        constituents = __attrValues x86_64-darwin.x86_64-darwin;
      };
      # --------------------------------------------------------------

    };
  }
  // (let
    x86_64-linux  = inputs.tullia.fromSimple "x86_64-linux"  (import ./nix/tullia.nix);
    x86_64-darwin = inputs.tullia.fromSimple "x86_64-darwin" (import ./nix/tullia.nix);
    fakeEvent = { inputs."GitHub event" = {id = ""; created_at = ""; value = {github_body.head_commit.id="0000000";};}; id=""; ociRegistry=""; };
  in {
    tullia.x86_64-linux = x86_64-linux.tullia;
    cicero.x86_64-linux = x86_64-linux.cicero;
    tullia.x86_64-darwin = x86_64-darwin.tullia;
    cicero.x86_64-darwin = x86_64-darwin.cicero;

    ciceroLocalTest.x86_64-linux  = (x86_64-linux.cicero."daedalus/ci"  fakeEvent).job;
    ciceroLocalTest.x86_64-darwin = (x86_64-darwin.cicero."daedalus/ci" fakeEvent).job;

  });
  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = ["https://cache.iog.io" "https://iog.cachix.org"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "iog.cachix.org-1:nYO0M9xTk/s5t1Bs9asZ/Sww/1Kt/hRhkLP0Hhv/ctY="];
    allow-import-from-derivation = "true";
  };
  # --------------------------------------------------------------
}
