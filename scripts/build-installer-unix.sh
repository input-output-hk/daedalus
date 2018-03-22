#!/usr/bin/env bash
# DEPENDENCIES (binaries should be in PATH):
#   0. 'git'
#   1. 'curl'
#   2. 'nix-shell'

set -e

CLUSTERS="$(cat $(dirname $0)/../installer-clusters.cfg | xargs echo -n)"

usage() {
    test -z "$1" || { echo "ERROR: $*" >&2; echo >&2; }
    cat >&2 <<EOF
  Usage:
    $0 DAEDALUS-VERSION CARDANO-BRANCH OPTIONS*

  Build a Daedalus installer.

  Options:
    --clusters "[CLUSTER-NAME...]"
                              Build installers for CLUSTERS.  Defaults to "mainnet staging"
    --fast-impure             Fast, impure, incremental build
    --build-id BUILD-NO       Identifier of the build; defaults to '0'

    --pull-request PR-ID      Pull request id we're building
    --nix-path NIX-PATH       NIX_PATH value

    --upload-s3               Upload the installer to S3
    --test-installer          Test the installer for installability

    --verbose                 Verbose operation
    --quiet                   Disable verbose operation

EOF
    test -z "$1" || exit 1
}

arg2nz() { test $# -ge 2 -a ! -z "$2" || usage "empty value for" $1; }
fail() { echo "ERROR: $*" >&2; exit 1; }
retry() {
        local tries=$1; arg2nz "iteration count" $1; shift
        for i in $(seq 1 ${tries})
        do if "$@"
           then return 0
           fi
           sleep 5s
        done
        fail "persistent failure to exec:  $*"
}

###
### Argument processing
###
fast_impure=
verbose=true
build_id=0
pull_request=
test_installer=

daedalus_version="$1"; arg2nz "daedalus version" $1; shift
cardano_branch="$(printf '%s' "$1" | tr '/' '-')"; arg2nz "Cardano SL branch to build Daedalus with" $1; shift

# Parallel build options for Buildkite agents only
if [ -n "${BUILDKITE_JOB_ID:-}" ]; then
    nix_shell="nix-shell --no-build-output --cores 0 --max-jobs 4"
else
    nix_shell="nix-shell --no-build-output"
fi

case "$(uname -s)" in
        Darwin ) OS_NAME=darwin; os=osx;   key=macos-3.p12;;
        Linux )  OS_NAME=linux;  os=linux; key=linux.p12;;
        * )     usage "Unsupported OS: $(uname -s)";;
esac

set -u ## Undefined variable firewall enabled
while test $# -ge 1
do case "$1" in
           --clusters )                                     CLUSTERS="$2"; shift;;
           --fast-impure )                               fast_impure=true;;
           --build-id )       arg2nz "build identifier" $2; build_id="$2"; shift;;
           --pull-request )   arg2nz "Pull request id" $2;
                                                        pull_request="--pull-request $2"; shift;;
           --nix-path )       arg2nz "NIX_PATH value" $2;
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
if test -n "${verbose}"
then set -x
fi

mkdir -p ~/.local/bin

rm -rf dist release node_modules || true

export PATH=$HOME/.local/bin:$PATH
export DAEDALUS_VERSION=${daedalus_version}.${build_id}
if [ -n "${NIX_SSL_CERT_FILE-}" ]; then export SSL_CERT_FILE=$NIX_SSL_CERT_FILE; fi

CARDANO_BUILD_UID="${OS_NAME}-${cardano_branch//\//-}"
ARTIFACT_BUCKET=ci-output-sink        # ex- cardano-sl-travis
CARDANO_ARTIFACT=cardano-binaries     # ex- daedalus-bridge
CARDANO_ARTIFACT_FULL_NAME=${CARDANO_ARTIFACT}-${CARDANO_BUILD_UID}

test -d node_modules/daedalus-client-api/ -a -n "${fast_impure}" || {
        retry 5 curl -o ${CARDANO_ARTIFACT_FULL_NAME}.tar.xz \
              "https://s3.eu-west-1.amazonaws.com/${ARTIFACT_BUCKET}/cardano-sl/${CARDANO_ARTIFACT_FULL_NAME}.tar.xz"
        mkdir -p node_modules/daedalus-client-api/
        du -sh  ${CARDANO_ARTIFACT_FULL_NAME}.tar.xz
        tar xJf ${CARDANO_ARTIFACT_FULL_NAME}.tar.xz --strip-components=1 -C node_modules/daedalus-client-api/
        rm      ${CARDANO_ARTIFACT_FULL_NAME}.tar.xz
        echo "cardano-sl build id is $(cat node_modules/daedalus-client-api/build-id)"
        if [ -f node_modules/daedalus-client-api/commit-id ]; then echo "cardano-sl revision is $(cat node_modules/daedalus-client-api/commit-id)"; fi
        if [ -f node_modules/daedalus-client-api/ci-url ]; then echo "cardano-sl ci-url is $(cat node_modules/daedalus-client-api/ci-url)"; fi
        pushd node_modules/daedalus-client-api
              mv log-config-prod.yaml cardano-node cardano-launcher configuration.yaml *genesis*.json ../../installers
        popd
        chmod +w installers/cardano-{node,launcher}
        strip installers/cardano-{node,launcher}
        rm -f node_modules/daedalus-client-api/cardano-*
}

test "$(find node_modules/ | wc -l)" -gt 100 -a -n "${fast_impure}" ||
        $nix_shell --run "npm install"

cd installers
    echo '~~~ Prebuilding dependencies for cardano-installer, quietly..'
    $nix_shell default.nix --run true || echo "Prebuild failed!"
    echo '~~~ Building the cardano installer generator..'
    INSTALLER=$(nix-build -j 2 --no-out-link)

    case ${OS_NAME} in
            darwin ) OS=macos64;;
            linux )  OS=linux;;esac
    for cluster in ${CLUSTERS}
    do
          echo "~~~ Generating installer for cluster ${cluster}.."
          export DAEDALUS_CLUSTER=${cluster}
                    INSTALLER_PKG="Daedalus-installer-${DAEDALUS_VERSION}-${cluster}.pkg"

          INSTALLER_CMD="$INSTALLER/bin/make-installer ${pull_request} ${test_installer}"
          INSTALLER_CMD+="  ${API:+--api $API}"
          INSTALLER_CMD+="  --build-job        ${BUILDKITE_BUILD_NUMBER}"
          INSTALLER_CMD+="  --cluster          ${cluster}"
          INSTALLER_CMD+="  --daedalus-version ${DAEDALUS_VERSION}"
          INSTALLER_CMD+="  --output           ${INSTALLER_PKG}"
          $nix_shell ../shell.nix --run "${INSTALLER_CMD}"

          APP_NAME="csl-daedalus"

          if test -f "${INSTALLER_PKG}"
          then
                  echo "~~~ Uploading the installer package.."
                  mkdir -p ${APP_NAME}
                  mv "${INSTALLER_PKG}" "${APP_NAME}/${INSTALLER_PKG}"

                  if [ -n "${BUILDKITE_JOB_ID:-}" ]
                  then
                          export PATH=${BUILDKITE_BIN_PATH:-}:$PATH
                          buildkite-agent artifact upload "${APP_NAME}/${INSTALLER_PKG}" s3://${ARTIFACT_BUCKET} --job $BUILDKITE_JOB_ID
                          rm "${APP_NAME}/${INSTALLER_PKG}"
                  fi
          else
                  echo "Installer was not made."
          fi
    done
cd ..

exit 0
