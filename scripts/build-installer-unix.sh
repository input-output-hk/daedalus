#!/usr/bin/env bash
set -e
source "$(dirname "$0")/utils.sh"

# DEPENDENCIES (binaries should be in PATH):
#   0. 'git'
#   1. 'curl'
#   2. 'nix-shell'

CLUSTERS="$(xargs echo -n < "$(dirname "$0")"/../installer-clusters.cfg)"

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
           --build-id )       validate_arguments "build identifier" "$2"; build_id="$2"; shift;;
           --nix-path )       validate_arguments "NIX_PATH value" "$2";
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
    retry 5 buildkite-agent artifact upload "$@" --job "$BUILDKITE_JOB_ID"
}

upload_artifacts_public() {
    retry 5 buildkite-agent artifact upload "$@" "${ARTIFACT_BUCKET:-}" --job "$BUILDKITE_JOB_ID"
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

pushd installers
    echo '~~~ Prebuilding dependencies for cardano-installer, quietly..'
    $nix_shell ../default.nix -A daedalus-installer --run true || echo "Prebuild failed!"
    echo '~~~ Building the cardano installer generator..'

    for cluster in ${CLUSTERS}
    do
          echo "~~~ Generating installer for cluster ${cluster}.."

          export DAEDALUS_CLUSTER="${cluster}"
          APP_NAME="csl-daedalus"
          rm -rf "${APP_NAME}"

          echo "Cluster type: cardano"
          CARDANO_BRIDGE="$(nix-build ../. --no-out-link -A daedalus-bridge --argstr nodeImplementation cardano --argstr cluster "${cluster}")"
          BRIDGE_FLAG="--cardano ${CARDANO_BRIDGE}"

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
                          if [ -n "${UPLOAD_DIR_OVERRIDE:-}" ] ; then
                            upload_dir="$UPLOAD_DIR_OVERRIDE"
                            mv "$APP_NAME" "$upload_dir"
                          else
                            upload_dir="$APP_NAME"
                          fi
                          upload_artifacts_public "${upload_dir}/*"
                          mv "launcher-config.yaml" "launcher-config-${cluster}.macos64.yaml"
                          upload_artifacts "launcher-config-${cluster}.macos64.yaml"
                          rm -rf "$upload_dir"
                  fi
          else
                  echo "Installer was not made."
          fi
    done
popd || exit 1

exit 0
