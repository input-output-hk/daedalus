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

    # Add treefmt to all devShells
    devShellsWithFormatter =
      lib.mapAttrs (
        name: shell:
          shell.overrideAttrs (oldAttrs: {
            buildInputs = (oldAttrs.buildInputs or []) ++ [config.treefmt.build.wrapper];
          })
      )
      devShells;
  in {
    devShells = devShellsWithFormatter;
  };
}
