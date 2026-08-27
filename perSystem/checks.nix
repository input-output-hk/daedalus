{inputs, ...}: {
  perSystem = {
    config,
    system,
    lib,
    pkgs,
    ...
  }: let
    # We reuse the pre-built node_modules from the installer pipeline so there's
    # no redundant yarn install. Resolved per-system rather than pinned to
    # x86_64-linux, so a check can run anywhere that pipeline already builds —
    # which is every system here, since the darwin installers depend on it.
    internal = inputs.self.internal.${system};
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
    checks =
      # The suites that execute the code under test run natively on each OS we
      # ship. They are full of platform branches — path separators, case
      # sensitivity, symlink and junction handling — and on a single platform
      # those branches are only exercised by overriding `process.platform`
      # in-process, which asserts what the code does when told it is elsewhere
      # rather than what it does when it is.
      #
      # Per OS, not per system: x86_64-darwin and aarch64-darwin share a kernel
      # and a filesystem, and nothing in these suites is architecture
      # dependent, so the second darwin run costs builder time for signal that
      # is already covered.
      lib.optionalAttrs (system != "x86_64-darwin") {
        jest = mkJsCheck "daedalus-jest" "yarn test:jest --maxWorkers=4";
        cucumber-unit = mkJsCheck "daedalus-cucumber-unit" "yarn test:unit";
      }
      # The rest are static analysis over the same source, and produce identical
      # results wherever they run. Repeating them per system would spend darwin
      # builder time for no additional signal.
      // lib.optionalAttrs (system == "x86_64-linux") {
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
        shellcheck = pkgs.callPackage ../tests/shellcheck.nix {src = inputs.self;};
        # `package.json` and `nix fmt` format the same files, so they must run
        # the same prettier. They are pinned independently: `devDependencies`
        # tracks the lockfile, and the formatter's prettier tracks nixpkgs, so a
        # nixpkgs bump moves one and not the other with no other signal.
        #
        # The version is read from the binary treefmt is configured to execute
        # rather than from a package named a second time here, so this check
        # cannot itself drift from the formatter it is checking.
        # The crypto assertions get their own named derivation so a reviewer
        # sees them pass or fail by name on a pull request rather than
        # inferring it from a generic Jest run. The `jest` check above already
        # executes these specs; this exists for visibility, and for the
        # coverage floor.
        #
        # The floor is here rather than in `jest.config.js` because a per-file
        # `coverageThreshold` there makes every partial `yarn jest <file>` run
        # exit 1, even when its own tests pass, since the threshold's file has
        # no coverage data in that run. Measured. Scoping it to this check
        # keeps the guarantee without that cost.
        #
        # What the floor buys: deleting a crypto test drops coverage below the
        # line and fails the build, rather than quietly reducing what is
        # checked. That is the move that kept a broken paper wallet path green
        # for five years.
        crypto-vectors = mkJsCheck "daedalus-crypto-vectors" ''
          yarn jest \
            source/renderer/app/utils/crypto.spec.ts \
            source/renderer/app/utils/entropy.spec.ts \
            --collectCoverageFrom='source/renderer/app/utils/{crypto,entropy}.ts' \
            --coverageThreshold='{
              "./source/renderer/app/utils/entropy.ts": {
                "statements": 100, "branches": 100, "functions": 100, "lines": 100
              },
              "./source/renderer/app/utils/crypto.ts": {
                "statements": 74.64, "branches": 77.77, "functions": 66.66, "lines": 75.4
              }
            }'
        '';
        prettier-version-parity = let
          pinned =
            (builtins.fromJSON (builtins.readFile ../package.json))
            .devDependencies
            .prettier;
          prettier = config.treefmt.settings.formatter.prettier.command;
        in
          pkgs.runCommand "daedalus-prettier-version-parity" {} ''
            actual=$(${prettier} --version)
            if [ "${pinned}" != "$actual" ]; then
              echo "ERROR: prettier version mismatch."
              echo "  package.json devDependencies.prettier: ${pinned}"
              echo "  prettier run by nix fmt:               $actual"
              echo
              echo "These format the same files and must be the same version."
              echo "Update package.json and yarn.lock to $actual, or pin the"
              echo "formatter's prettier to ${pinned}."
              exit 1
            fi
            echo "prettier ${pinned} in package.json and in nix fmt"
            touch $out
          '';
      };
  };
}
