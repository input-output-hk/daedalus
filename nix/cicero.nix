{inputs}: let
  tullia = (import inputs.flake-compat { src = inputs.tullia.outPath; }).defaultNix;
  # For now, Cicero is building for Darwin on Linux with configured remote builders:
  x86_64-linux = tullia.fromSimple "x86_64-linux" (import ./tullia.nix);
in {
  tullia.x86_64-linux = x86_64-linux.tullia;
  cicero.x86_64-linux = x86_64-linux.cicero;
  tulliaLocalTest.x86_64-linux = let
    pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
  in
    pkgs.linkFarm "tulliaLocalTest" (pkgs.lib.mapAttrsToList (k: v: {
        name = k;
        path = v.computedCommand;
      })
      x86_64-linux.tullia.task);
}
