{inputs, ...}: {
  perSystem = {
    config,
    system,
    lib,
    pkgs,
    ...
  }: let
    devShells = import ../nix/devshells.nix {
      inherit inputs;
      targetSystem = system;
    };

    # Add treefmt (and drt when available) to all devShells.
    devShellsWithExtras =
      lib.mapAttrs (
        _name: shell:
          shell.overrideAttrs (oldAttrs: {
            buildInputs =
              (oldAttrs.buildInputs or [])
              ++ [config.treefmt.build.wrapper]
              ++ lib.optional (config.packages ? drt) config.packages.drt;
          })
      )
      devShells;
  in {
    devShells = devShellsWithExtras;
  };
}
