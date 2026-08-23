{
  inputs,
  targetSystem,
}: let
  buildSystem =
    {
      x86_64-windows = "x86_64-linux";
    }.${
      targetSystem
    } or targetSystem;

  targetSuffix =
    if buildSystem == targetSystem
    then ""
    else "-${targetSystem}";

  pkgs = inputs.nixpkgs.legacyPackages.${buildSystem};

  inherit (pkgs) lib;
  inherit (inputs.self.internal) installerClusters;
in
  pkgs.writeShellScriptBin "buildkite-pipeline" ''
    set -o errexit
    set -o pipefail

    # We need to echo to stdout here, to prevent a Buildkite timing bug:
    echo …

    echo '~~~ Will make ‘${targetSystem}’ installers: ${lib.concatMapStringsSep ", " (s: "‘${s}’") installerClusters}'
    echo …

    retry() {
      local tries=$1
      shift
      while [ "$tries" -gt 0 ] ; do
        tries=$((tries - 1))
        if "$@" ; then
          return 0
        elif [ "$tries" = 0 ] ; then
          echo >&2 "fatal: failed to run ‘$*’"
          return 1
        else
          echo >&2 "error: failed to run ‘$*’, will retry $tries more time(s)"
          sleep 5
        fi
      done
    }

    userNixConfig="$HOME/.config/nix/nix.conf"
    if [ -e "$userNixConfig" ] ; then
      echo "~~~ Warning: cleaning user’s Nix config: $userNixConfig"
      echo "Sometimes, a conflicting nix.conf appears in ~/.config/nix, which"
      echo "results in builds not using our global substituters (binary caches)."
      echo
      mv -v "$userNixConfig" "$userNixConfig.$(date -Iseconds)"
    fi

    # *Maybe* prevent segfaults on `aarch64-darwin` in `GC_*` code:
    export GC_DONT_GC=1 # <https://chromium.googlesource.com/chromiumos/third_party/gcc/+/f4131b9cddd80547d860a6424ee1644167a330d6/gcc/gcc-4.6.0/boehm-gc/doc/README.environment#151>

    ${lib.concatMapStringsSep "" (cluster: ''
        echo '~~~ Generating installer for cluster ‘${cluster}’'

        tmpdir=$(mktemp -d)

        # XXX: `set -x` to give CI users a reproduction, and `| cat`:
        #   • turns off any interactive questions from Nix (e.g. accept-flake-config)
        #   • turns off the progress bar which bloats raw logs
        #   • keeps `derivation-name> ` prefix in logs
        #   • but also kills colors :-(

        ${
          if targetSystem == "x86_64-linux"
          then ''
            debResult="$tmpdir"/csl-daedalus-deb
            (
              set -x
              nix build --no-accept-flake-config -L --out-link "$debResult" .#packages.${buildSystem}.deb-installer-${cluster}
            ) 2>&1 | cat
            echo "Built .deb: $(readlink "$debResult")"

            rpmResult="$tmpdir"/csl-daedalus-rpm
            (
              set -x
              nix build --no-accept-flake-config -L --out-link "$rpmResult" .#packages.${buildSystem}.rpm-installer-${cluster}
            ) 2>&1 | cat
            echo "Built .rpm: $(readlink "$rpmResult")"
          ''
          else ''
            result="$tmpdir"/csl-daedalus
            (
              set -x
              nix build --no-accept-flake-config -L --out-link "$result" .#packages.${buildSystem}.installer-${cluster}${targetSuffix}
            ) 2>&1 | cat
            echo "Built: $(readlink "$result")"
          ''
        }

        if [ -n "''${BUILDKITE_JOB_ID:-}" ]; then
          ${
          if targetSystem == "x86_64-linux"
          then ''
            echo '~~~ Uploading Linux packages for cluster ‘${cluster}’'
            (
              # Keep the artifact globs format-specific so no portable output can be uploaded.
              cd "$tmpdir"
              retry 5 buildkite-agent artifact upload "csl-daedalus-deb/*.deb" "''${ARTIFACT_BUCKET:-}" --job "$BUILDKITE_JOB_ID"
              retry 5 buildkite-agent artifact upload "csl-daedalus-rpm/*.rpm" "''${ARTIFACT_BUCKET:-}" --job "$BUILDKITE_JOB_ID"
            )
          ''
          else ''
            ${lib.optionalString (targetSystem == "x86_64-darwin" || targetSystem == "aarch64-darwin") ''
              echo '~~~ Signing installer for cluster ‘${cluster}’'
              nix run -L .#packages.${buildSystem}.makeSignedInstaller-${cluster}${targetSuffix} | tee make-installer.log
              rm "$result"
              mkdir -p "$result"
              mv $(tail -n 1 make-installer.log) "$result"/
            ''}

            echo '~~~ Uploading installer for cluster ‘${cluster}’'
            (
              # XXX: we have to chdir, since buildkite-agent uploads keeping full path
              cd "$tmpdir"
              retry 5 buildkite-agent artifact upload */*-${targetSystem}.* "''${ARTIFACT_BUCKET:-}" --job "$BUILDKITE_JOB_ID"
            )
          ''
        }
        fi

        rm -r "$tmpdir"
      '')
      installerClusters}
  ''
