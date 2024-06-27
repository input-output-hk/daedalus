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

target="$HOME"/.daedalus/@CLUSTER@
if [ -e "$target" ] ; then
  echo "Found an older version of Daedalus "@CLUSTER@", removing it..."
  chmod -R +w "$target"
  rm -rf "$target"
fi
mkdir -p "$target"

old_nix="$HOME"/.daedalus/nix
if [ -e "$old_nix" ] ; then
  old_clusters=$(ls "$old_nix"/var/nix/profiles/ | grep '^profile-' | grep -v '[0-9]' || true)
  if [ "$old_clusters" = "profile-"@CLUSTER@ ] ; then
    # If the user *only* used Mainnet (most common), we're safe to remove the whole ~/.daedalus/nix:
    echo "Found an older non-portable version of Daedalus in $old_nix, removing it..."
    chmod -R +w "$old_nix"
    rm -rf "$old_nix"
  else
    # But if it contains more Daedaluses for other networks, we can't risk breaking them:
    echo "Found older non-portable versions of Daedalus for multiple networks in $old_nix, you are free to remove the directory manually, if you no longer use them."
  fi
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

echo "Setting up a .desktop entry..."
mkdir -p "$HOME"/.local/share/applications
chmod -R +w "$target"/share/applications
sed -r "s+INSERT_PATH_HERE+$(echo "$target"/bin/*)+g" -i "$target"/share/applications/*.desktop
sed -r "s+INSERT_ICON_PATH_HERE+$target/share/icon_large.png+g" -i "$target"/share/applications/*.desktop
chmod -R -w "$target"/share/applications
ln -sf "$target"/share/applications/*.desktop "$HOME"/.local/share/applications/Daedalus-@CLUSTER@.desktop

echo "Installed successfully!"
echo
echo "Now, either:"
echo "  1. In a terminal, run $(echo "$target"/bin/* | sed -r "s+^$HOME+~+")"
echo "  2. Or select Start -> Daedalus "@CLUSTER@"."

exit 0
