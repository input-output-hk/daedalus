{ inputs, targetSystem }:

let

  inherit (inputs.self.outputs.packages.${targetSystem}.internal.mainnet.oldCode.pkgs)
    lib writeShellScriptBin;
  inherit (import ./source-lib.nix { inherit inputs; }) installerClusters;

  # FIXME: change all to `csl-daedalus` once we get official aarch64-darwin workers:
  bucketSubdir = if targetSystem != "aarch64-darwin" then "csl-daedalus" else "UNSAFE-internal-build";

in

writeShellScriptBin "buildkite-pipeline" ''
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
    result="$tmpdir"/${bucketSubdir}

    # XXX: this `2>&1 | cat` part below:
    #  • turns off progress bar, which clutters the raw logs,
    #  • but also keeps `derivation-name> ` prefix in logs
    #  • but also unfortunately kills colors :'(
    # Another option would be to patch the `nix` binary used here:

    nix build --no-accept-flake-config -L --out-link "$result" .#packages.${targetSystem}.installer.${cluster} 2>&1 | cat

    echo "Built: $(readlink "$result")"

    if [ -n "''${BUILDKITE_JOB_ID:-}" ]; then
      ${if targetSystem != "x86_64-darwin" then "" else ''
        echo '~~~ Signing installer for cluster ‘${cluster}’'
        nix run -L .#packages.${targetSystem}.makeSignedInstaller.${cluster} | tee make-installer.log
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
    fi

    rm -r "$tmpdir"
  '') installerClusters}
''
