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
    inherit (internal) nodejs yarn srcWithoutNix node_modules;

    mkJsCheck = name: command:
      pkgs.stdenv.mkDerivation {
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
      # `yarn i18n:manage` regenerates translation artifacts that are tracked in
      # the repository. Running it is not enough on its own: the command exits 0
      # whether or not the regenerated output matches what is committed, so the
      # tracked artifacts can drift from source indefinitely with CI green.
      # Snapshot them, regenerate, and require the result to be identical.
      i18n = mkJsCheck "daedalus-i18n" ''
        cp -r source/renderer/app/i18n/locales .i18n-locales-before
        cp translations/messages.json .i18n-messages-before.json

        yarn i18n:manage

        if ! diff -r .i18n-locales-before source/renderer/app/i18n/locales \
          || ! diff .i18n-messages-before.json translations/messages.json; then
          echo
          echo "ERROR: committed translation artifacts are out of date."
          echo "Run 'yarn i18n:manage' and commit the resulting changes."
          exit 1
        fi

        rm -rf .i18n-locales-before .i18n-messages-before.json
      '';
      storybook = mkJsCheck "daedalus-storybook-build" "yarn storybook:build";
      jest = mkJsCheck "daedalus-jest" "yarn test:jest --maxWorkers=4";
      cucumber-unit = mkJsCheck "daedalus-cucumber-unit" "yarn test:unit";
      shellcheck = pkgs.callPackage ../tests/shellcheck.nix {src = inputs.self;};

      # The portable bundle declares things about itself — paths its launcher
      # exports, libraries it needs, links it contains — and nothing verified
      # those declarations against the directory actually produced. Both recent
      # release failures were "build succeeds, application does not start",
      # which no other check in this set can see.
      #
      # The self-test runs first: it breaks a fixture bundle one way at a time
      # and requires the checker to reject each break. A checker that has
      # silently stopped asserting anything then fails here, instead of passing
      # every bundle put in front of it.
      bundle-integrity =
        pkgs.runCommand "daedalus-bundle-integrity" {
          # glibc.static: the self-test's fixture binary is linked statically so
          # the fixture bundle is genuinely self-contained and its library
          # closure does not depend on what the builder happens to provide.
          nativeBuildInputs = [pkgs.binutils pkgs.stdenv.cc pkgs.glibc.static];
        } ''
          # Invoked through bash rather than executed directly: the sandbox has
          # no /usr/bin/env for the scripts' shebangs to resolve.
          bash ${inputs.self}/scripts/check-bundle-integrity-selftest.sh \
            ${inputs.self}/scripts/check-bundle-integrity.sh
          echo
          bash ${inputs.self}/scripts/check-bundle-integrity.sh \
            ${internal.relocatableElectron}
          touch $out
        '';
    };
  };
}
