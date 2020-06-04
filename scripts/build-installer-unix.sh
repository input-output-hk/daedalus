#!/usr/bin/env bash
# DEPENDENCIES (binaries should be in PATH):
#   0. 'git'
#   1. 'curl'
#   2. 'nix-shell'

set -e

CLUSTERS="$(xargs echo -n < "$(dirname "$0")"/../installer-clusters.cfg)"

usage() {
    test -z "$1" || { echo "ERROR: $*" >&2; echo >&2; }
    cat >&2 <<EOF
  Usage:
    $0 OPTIONS*

  Build a Daedalus installer.

  Options:
    --clusters "[CLUSTER-NAME...]"
                              Build installers for CLUSTERS.  Defaults to "mainnet staging testnet"
    --fast-impure             Fast, impure, incremental build
    --build-id BUILD-NO       Identifier of the build; defaults to '0'

    --nix-path NIX-PATH       NIX_PATH value

    --upload-s3               Upload the installer to S3
    --test-installer          Test the installer for installability

    --verbose                 Verbose operation
    --quiet                   Disable verbose operation

EOF
    test -z "$1" || exit 1
}

arg2nz() { test $# -ge 2 -a ! -z "$2" || usage "empty value for" "$1"; }
fail() { echo "ERROR: $*" >&2; exit 1; }
retry() {
        local tries=$1; arg2nz "iteration count" "$1"; shift
        for i in $(seq 1 "${tries}")
        do if "$@"
           then return 0
           else echo "failed, retry #$i of ${tries}"
           fi
           sleep 5s
        done
        fail "persistent failure to exec:  $*"
}

###
### Argument processing
###
fast_impure=
verbose=
build_id=0
test_installer=
code_signing_config=
signing_config=

# Parallel build options for Buildkite agents only
if [ -n "${BUILDKITE_JOB_ID:-}" ]; then
    nix_shell="nix-shell --no-build-output --cores 0 --max-jobs 4"
else
    nix_shell="nix-shell --no-build-output"
fi

case "$(uname -s)" in
        Darwin ) export OS_NAME=darwin; export os=osx;   export key=macos-3.p12;;
        Linux )  export OS_NAME=linux;  export os=linux; export key=linux.p12;;
        * )     usage "Unsupported OS: $(uname -s)";;
esac

set -u ## Undefined variable firewall enabled
while test $# -ge 1
do case "$1" in
           --clusters )                                     CLUSTERS="$2"; shift;;
           --fast-impure )                               export fast_impure=true;;
           --build-id )       arg2nz "build identifier" "$2"; build_id="$2"; shift;;
           --nix-path )       arg2nz "NIX_PATH value" "$2";
                                                     export NIX_PATH="$2"; shift;;
           --test-installer )                         test_installer="--test-installer";;

           ###
           --verbose )        echo "$0: --verbose passed, enabling verbose operation"
                                                             verbose=t;;
           --quiet )          echo "$0: --quiet passed, disabling verbose operation"
                                                             verbose=;;
           --help )           usage;;
           "--"* )            usage "unknown option: '$1'";;
           * )                break;; esac
   shift; done

set -e
echo "${verbose}"
if test -n "${verbose}"
then set -x
fi

if [ -f /var/lib/buildkite-agent/code-signing-config.json ]; then
  code_signing_config="--code-signing-config /var/lib/buildkite-agent/code-signing-config.json"
fi

if [ -f /var/lib/buildkite-agent/signing-config.json ]; then
  signing_config="--signing-config /var/lib/buildkite-agent/signing-config.json"
fi

export daedalus_version="${1:-dev}"

mkdir -p ~/.local/bin

if test -e "dist" -o -e "release" -o -e "node_modules"
then rm -rf dist release node_modules || true
fi

export PATH=$HOME/.local/bin:$PATH
if [ -n "${NIX_SSL_CERT_FILE-}" ]; then export SSL_CERT_FILE=$NIX_SSL_CERT_FILE; fi

upload_artifacts() {
    buildkite-agent artifact upload "$@" --job "$BUILDKITE_JOB_ID"
}

upload_artifacts_public() {
    buildkite-agent artifact upload "$@" "${ARTIFACT_BUCKET:-}" --job "$BUILDKITE_JOB_ID"
}

function checkItnCluster() {
  for c in $2
  do
    if [[ "${c}" == "${1}" ]]
    then
      echo 1
    fi
  done
}

# Build/get cardano bridge which is used by make-installer
echo '~~~ Prebuilding cardano bridge'
CARDANO_BRIDGE=$(nix-build --no-out-link -A daedalus-bridge --argstr nodeImplementation cardano)
echo '~~~ Prebuilding jormungandr bridge'
JORMUNGANDR_BRIDGE=$(nix-build --no-out-link -A daedalus-bridge --argstr nodeImplementation jormungandr)

itnClusters="$(< "$(nix-build --no-out-link -A itnClustersFile)")"

pushd installers
    echo '~~~ Prebuilding dependencies for cardano-installer, quietly..'
    $nix_shell ../default.nix -A daedalus-installer --run true || echo "Prebuild failed!"
    echo '~~~ Building the cardano installer generator..'

    for cluster in ${CLUSTERS}
    do
          echo "~~~ Generating installer for cluster ${cluster}.."
          LAUNCHER_CONFIG="$(nix-build ../. --no-out-link -A launcherConfigs.configFiles --argstr cluster "${cluster}")/launcher-config.yaml"
          KIND="$($nix_shell ../shell.nix -A buildShell --run "jq .nodeConfig.kind < $LAUNCHER_CONFIG ")"

          export DAEDALUS_CLUSTER="${cluster}"
          APP_NAME="csl-daedalus"
          rm -rf "${APP_NAME}"

          if [ "$(checkItnCluster "${cluster}" "${itnClusters}")" == "1" ]; then
            echo "Cluster type: jormungandr"
            BRIDGE_FLAG="--jormungandr ${JORMUNGANDR_BRIDGE}"
          else
            echo "Cluster type: cardano"
            CARDANO_BRIDGE="$(nix-build ../. --no-out-link -A daedalus-bridge --argstr nodeImplementation cardano --argstr cluster "${cluster}")"
            BRIDGE_FLAG="--cardano-${KIND} ${CARDANO_BRIDGE}"
          fi

          INSTALLER_CMD=("make-installer"
                         "${test_installer}"
                         "${code_signing_config}"
                         "${signing_config}"
                         "${BRIDGE_FLAG}"
                         "  --build-job        ${build_id}"
                         "  --cluster          ${cluster}"
                         "  --out-dir          ${APP_NAME}")
          nix-build .. -A launcherConfigs.configFiles --argstr os macos64 --argstr cluster "${cluster}" -o cfg-files
          cp -v cfg-files/* .
          chmod -R +w .
          echo 'Running make-installer in nix-shell'
          $nix_shell ../shell.nix -A buildShell --run "${INSTALLER_CMD[*]}"

          if [ -d ${APP_NAME} ]; then
                  if [ -n "${BUILDKITE_JOB_ID:-}" ]
                  then
                          echo "Uploading the installer package.."
                          export PATH=${BUILDKITE_BIN_PATH:-}:$PATH
                          upload_artifacts_public "${APP_NAME}/*"
                          mv "launcher-config.yaml" "launcher-config-${cluster}.macos64.yaml"
                          upload_artifacts "launcher-config-${cluster}.macos64.yaml"
                          rm -rf "${APP_NAME}"
                  fi
          else
                  echo "Installer was not made."
          fi
    done
popd || exit 1

exit 0
