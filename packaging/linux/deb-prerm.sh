#!/bin/sh
# shellcheck source=linux-deb-common.sh
. "$(dirname "$0")/linux-deb-common.sh"

lock
case "${1:-}" in
  upgrade|failed-upgrade|deconfigure) exit 0 ;;
  remove)
    : >"$state_dir/removing"
    chmod 0600 "$state_dir/removing"
    electron="$install_root/libexec/bundle-electron/lib/electron/electron"
    for proc_exe in /proc/[0-9]*/exe; do
      [ -e "$proc_exe" ] || continue
      [ "$(readlink "$proc_exe" 2>/dev/null || true)" != "$electron" ] || fail 'close Daedalus before removing the package'
    done
    ;;
  *) fail "unsupported prerm transition: ${1:-missing}" ;;
esac
