# DEPENDENCIES (binaries should be in PATH):
#   0. 'git'
#   1. 'curl'
#   2. 'nix-shell'
#   3. 'stack'

DEFAULT_DAEDALUS_BRANCH=master

DAEDALUS_BRANCH=${1:-${DEFAULT_DAEDALUS_BRANCH}}
GITHUB_USER=${2:-input-output-hk}
shift 2

URL=https://github.com/${GITHUB_USER}/daedalus.git

test ! -e daedalus.old ||
        rm -rf daedalus.old
mv daedalus daedalus.old 2>/dev/null

set -e -u

test -n "$(type -P nix-shell)" || {
        cat <<EOF
WARNING: 'nix-shell' is absent from PATH

Installation can be performed by following instructions at:

  https://nixos.org/nix/download.html

..or, if you're willing to skip straight to action:

  curl https://nixos.org/nix/install | sh

..are you willing to perform the above command?

EOF
        echo -n "Confirm: Y / n? "
        read ans
        test "${ans}" = "Y" -o "${ans}" = "y" -o -z "${ans}" || {
                echo "FATAL: 'nix-shell' unavailable and user declined to proceed with installation." >&2
                exit 1
        }
        echo "INFO:  proceeding with Nix installation, hang on tight."
        echo
        curl https://nixos.org/nix/install | sh
        echo '. ~/.nix-profile/etc/profile.d/nix.sh' >> ~/.profile
        . ~/.profile
}

echo "Building Daedalus branch ${DAEDALUS_BRANCH} from ${URL}"
git clone ${URL}

pushd daedalus
    git reset --hard origin/${DAEDALUS_BRANCH}

    scripts/build-installer-unix.sh \
            "${GITHUB_USER}-${DAEDALUS_BRANCH}-$(git show-ref --hash HEAD)" \
            "$@"
popd
