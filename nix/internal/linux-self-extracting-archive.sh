#!/bin/sh

# XXX: Be *super careful* changing this!!! You WILL DELETE user data if you make a mistake.

# XXX: no -o pipefail in dash (on debians)
set -eu

skip_bytes=$(( 1010101010 - 1000000000 ))

echo "Verifying SHA-256 checksum..."
our_checksum=$(tail -c+$((skip_bytes+1)) "$0" | sha256sum | cut -d' ' -f1)
if [ "$our_checksum" != 0000000000000000000000000000000000000000000000000000000000000000 ] ; then
  echo "Checksum verification failure. Please, download the installer again."
  exit 1
fi

# We could be running as an auto-update from Daedalus ≤5.4.0 using `nix-chroot`,
# then our behavior should be different:
in_chroot=
if [ "${1-}" = "--extract" ] && [ -e /nix/var/nix/profiles/profile-@CLUSTER@ ] ; then
  in_chroot=1
fi

target="$HOME"/.daedalus/@CLUSTER@
if [ -e "$target" ] ; then
  echo "Found an older version of Daedalus "@CLUSTER@", removing it..."
  chmod -R +w "$target"
  rm -rf "$target"
fi
mkdir -p "$target"

if [ -z "$in_chroot" ] ; then
  @REMOVE_OLD_NIX_CHROOT@
fi

progress_cmd="cat"
if type pv >/dev/null ; then
  total_size=$(stat -c "%s" "$0")
  progress_cmd="pv -s "$((total_size - skip_bytes))
else
  echo "Note: you don't have \`pv' installed, so we can't show progress"
fi

echo "Unpacking..."
tail -c+$((skip_bytes+1)) "$0" | $progress_cmd | tar -C "$target" -xJ

# Move a faux `satisfyOldUpdateRunner` to $PWD so that the old `update-runner` doesn’t error out:
chmod +w "$target"
chmod -R +w "$target"/dat
if [ -z "$in_chroot" ] ; then
  rm -rf "$target"/dat
else
  mv "$target"/dat ./
fi
chmod -w "$target"

echo "Setting up a .desktop entry..."
mkdir -p "$HOME"/.local/share/applications
chmod -R +w "$target"/share/applications
sed -r "s+INSERT_PATH_HERE+$(echo "$target"/bin/*)+g" -i "$target"/share/applications/*.desktop
sed -r "s+INSERT_ICON_PATH_HERE+$target/share/icon_large.png+g" -i "$target"/share/applications/*.desktop
chmod -R -w "$target"/share/applications
ln -sf "$target"/share/applications/*.desktop "$HOME"/.local/share/applications/Daedalus-@CLUSTER@.desktop

# Backwards compatibility:
XDG_DATA_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}"
DAEDALUS_DIR="${XDG_DATA_HOME}/Daedalus"
mkdir -p "$DAEDALUS_DIR"/@CLUSTER@/
ln -sf "$target"/bin/daedalus "$DAEDALUS_DIR"/@CLUSTER@/namespaceHelper

if [ -z "$in_chroot" ] ; then
  echo "Installed successfully!"
  echo
  echo "Now, either:"
  echo "  1. In a terminal, run $(echo "$target"/bin/* | sed -r "s+^$HOME+~+")"
  echo "  2. Or select Start -> Daedalus "@CLUSTER@"."
else
  # Now, the currently running `cardano-launcher` will try to restart `daedalus-frontend`. See the
  # new `daedalus-frontend` stub in `satisfyOldUpdateRunner`.
  :
fi

exit 0
