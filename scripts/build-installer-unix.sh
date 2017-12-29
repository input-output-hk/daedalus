#!/usr/bin/env bash
# DEPENDENCIES (binaries should be in PATH):
#   0. 'git'
#   1. 'curl'
#   2. 'nix-shell'
#   3. 'stack'

set -e

usage() {
    test -z "$1" || { echo "ERROR: $*" >&2; echo >&2; }
    cat >&2 <<EOF
  Usage:
    $0 DAEDALUS-VERSION CARDANO-BRANCH OPTIONS*

  Build a Daedalus installer.

  Options:
    --fast-impure             Fast, impure, incremental build
    --build-id BUILD-NO       Identifier of the build; defaults to '0'

    --pr-id PR-ID             Pull request id we're building
    --nix-path NIX-PATH       NIX_PATH value

    --upload-s3               Upload the installer to S3
    --test-install            Test the installer for installability

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
pr_id=true
upload_s3=
test_install=

daedalus_version="$1"; arg2nz "daedalus version" $1; shift
cardano_branch="$(printf '%s' "$1" | tr '/' '-')"; arg2nz "Cardano SL branch to build Daedalus with" $1; shift

case "$(uname -s)" in
        Darwin ) OS_NAME=darwin; os=osx;   key=macos-3.p12;;
        Linux )  OS_NAME=linux;  os=linux; key=linux.p12;;
        * )     usage "Unsupported OS: $(uname -s)";;
esac

set -u ## Undefined variable firewall enabled
while test $# -ge 1
do case "$1" in
           --fast-impure )                               fast_impure=true;;
           --build-id )       arg2nz "build identifier" $2;    build_id="$2"; shift;;
           --pr-id )          arg2nz "Pull request id" $2;
                                                               pr_id="$2"; shift;;
           --nix-path )       arg2nz "NIX_PATH value" $2;
                                                     export NIX_PATH="$2"; shift;;
           --upload-s3 )                                   upload_s3=t;;
           --test-install )                             test_install=t;;

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
        nix-shell --no-build-output --cores 0 --max-jobs 4 --run "npm install"

test -d "release/darwin-x64/Daedalus-darwin-x64" -a -n "${fast_impure}" || {
        nix-shell --no-build-output --cores 0 --max-jobs 4 --run "npm run package -- --icon installers/icons/256x256.png"
        echo "Size of Electron app is $(du -sh release)"
}

test -n "$(which stack)"     -a -n "${fast_impure}" ||
        retry 5 bash -c "curl -L https://www.stackage.org/stack/${os}-x86_64 | \
                         tar xz --strip-components=1 -C ~/.local/bin"

cd installers
    # if test "${pr_id}" = "false" -a "${OS_NAME}" != "linux" # No Linux keys yet.
    # then retry 5 nix-shell -p awscli --no-build-output --cores 0 --max-jobs 4 --run "aws s3 cp --region eu-central-1 s3://iohk-private/${key} macos.p12"
    # fi
    echo "Prebuilding dependencies for cardano-installer, quietly.."
    ## XXX: temporarily disable signing
    export BUILDKITE_PULL_REQUEST=""
    nix-shell --run true --no-build-output --cores 0 --max-jobs 4 default.nix ||
        echo "Prebuild failed!"
    echo "Building and running the cardano installer generator.."
    $(nix-build -j 2)/bin/make-installer
    echo "Uploading the installer package.."
    cd dist
    APP_NAME="csl-daedalus"
    mkdir -p ${APP_NAME}
    mv "Daedalus-installer-${DAEDALUS_VERSION}.pkg" ${APP_NAME}/"Daedalus-installer-${DAEDALUS_VERSION}.pkg"
    buildkite-agent artifact upload ${APP_NAME}/Daedalus-installer-${DAEDALUS_VERSION}.pkg s3://${ARTIFACT_BUCKET} --job $BUILDKITE_JOB_ID
    cd ..
    # if test -n "${upload_s3}"
    # then
    #         echo "$0: --upload-s3 passed, will upload the installer to S3";
    #         retry 5 nix-shell -p awscli --run "aws s3 cp 'dist/Daedalus-installer-${DAEDALUS_VERSION}.pkg' s3://daedalus-internal/ --acl public-read"
    # fi
    if test -n "${test_install}"
    then echo "$0:  --test-install passed, will test the installer for installability";
         case ${OS_NAME} in
                 darwin ) sudo installer -dumplog -verbose -target / -pkg "dist/Daedalus-installer-${DAEDALUS_VERSION}.pkg";;
                 linux )  echo "WARNING: installation testing not implemented on Linux" >&2;; esac; fi
cd ..

ls -la installers/dist

exit 0
