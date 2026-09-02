#!/bin/sh
# shellcheck source=linux-deb-common.sh
. "$(dirname "$0")/linux-deb-common.sh"

lock
case "${1:-}" in
  install)
    [ ! -L "$install_root" ] || fail 'refusing symlinked install root'
    snapshot_state
    ;;
  upgrade)
    old_version=${2:-0}
    if ! dpkg --compare-versions "$package_version" gt "$old_version"; then
      current_status=$(dpkg-query -W -f='${db:Status-Status}' "daedalus-$cluster" 2>/dev/null || true)
      configured_version=$(cat "$state_dir/configured-version" 2>/dev/null || true)
      [ "$current_status" != installed ] && [ "$package_version" = "$configured_version" ] ||
        fail 'package downgrade refused'
    fi
    snapshot_state
    ;;
  abort-upgrade)
    restore_state
    ;;
  *) fail "unsupported preinst transition: ${1:-missing}" ;;
esac
