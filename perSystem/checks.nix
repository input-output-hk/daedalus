{inputs, ...}: {
  perSystem = {
    system,
    lib,
    pkgs,
    ...
  }: let
    # JS/TS checks only need to run on one platform — x86_64-linux is cheapest.
    # We reuse the pre-built node_modules from the installer pipeline so there's
    # no redundant yarn install.
    internal = inputs.self.internal.x86_64-linux;
    pkgsJs = internal.common.pkgsJs;
    inherit (internal) nodejs yarn srcWithoutNix node_modules;

    mkJsCheck = name: command:
      pkgsJs.stdenv.mkDerivation {
        inherit name;
        src = srcWithoutNix;
        nativeBuildInputs = [yarn nodejs];
        configurePhase = ''
          export HOME=$(realpath $NIX_BUILD_TOP/home)
          mkdir -p $HOME
          # Prevent yarn from hitting the network in the sandbox:
          echo '"--offline" true' >>$HOME/.yarnrc
          cp -r ${node_modules}/. ./
          chmod -R +w .
          patchShebangs .
        '';
        buildPhase = command;
        installPhase = "touch $out";
        dontFixup = true;
      };
  in {
    checks = lib.optionalAttrs (system == "x86_64-linux") {
      lint = mkJsCheck "daedalus-lint" "yarn lint";
      compile = mkJsCheck "daedalus-compile" "yarn compile";
      stylelint = mkJsCheck "daedalus-stylelint" "yarn stylelint";
      i18n = mkJsCheck "daedalus-i18n" "yarn i18n:manage";
      storybook = mkJsCheck "daedalus-storybook-build" "yarn storybook:build";
      jest = mkJsCheck "daedalus-jest" "yarn test:jest --maxWorkers=4";
      shellcheck = pkgs.callPackage ../tests/shellcheck.nix {src = inputs.self;};
    };
  };
}
