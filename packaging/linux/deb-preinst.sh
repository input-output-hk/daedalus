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
    dpkg --compare-versions "$package_version" gt "$old_version" || fail 'package downgrade refused'
    snapshot_state
    ;;
  abort-upgrade)
    restore_state
    ;;
  *) fail "unsupported preinst transition: ${1:-missing}" ;;
esac
