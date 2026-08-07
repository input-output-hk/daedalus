# The portable bundle declares things about itself — paths its launcher
# exports, libraries it needs, links it contains — and nothing verified those
# declarations against the directory actually produced. Both recent release
# failures were "build succeeds, application does not start", which no other
# check in this set can see.
#
# This lives in its own module rather than in checks.nix because it is the only
# check tied to a specific system's build products: it inspects the Linux
# relocatable bundle and needs ELF tooling. flake-parts merges `checks` across
# perSystem modules, so it joins the same set and becomes merge-blocking the
# same way.
{inputs, ...}: {
  perSystem = {
    system,
    lib,
    pkgs,
    ...
  }: {
    checks = lib.optionalAttrs (system == "x86_64-linux") {
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
            ${inputs.self.internal.x86_64-linux.relocatableElectron}
          touch $out
        '';
    };
  };
}
