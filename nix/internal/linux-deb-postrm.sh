#!/bin/sh
# shellcheck source=linux-deb-common.sh
. "$(dirname "$0")/linux-deb-common.sh"

lock
case "${1:-}" in
  upgrade|disappear) exit 0 ;;
  abort-install|abort-upgrade|failed-upgrade)
    restore_state
    ;;
  remove|purge)
    if [ -e "$profile_path" ] || [ -L "$profile_path" ]; then
      assert_regular_file "$profile_path"
      [ -f "$state_dir/profile.sha256" ] || fail 'preserving foreign AppArmor profile without ownership marker'
      [ "$(sha256sum "$profile_path" | cut -d' ' -f1)" = "$(cat "$state_dir/profile.sha256")" ] || fail 'preserving modified AppArmor profile and package state'
      command -v apparmor_parser >/dev/null 2>&1 || fail 'cannot safely unload AppArmor profile'
      apparmor_parser -R "$profile_path" || fail 'failed to unload package AppArmor profile'
      rm -f "$profile_path" "$state_dir/profile.sha256"
    fi
    if [ -e "$manifest_path" ] || [ -L "$manifest_path" ]; then
      assert_regular_file "$manifest_path"
      [ -f "$state_dir/manifest.sha256" ] || fail 'preserving foreign sandbox manifest without ownership marker'
      [ "$(sha256sum "$manifest_path" | cut -d' ' -f1)" = "$(cat "$state_dir/manifest.sha256")" ] || fail 'preserving modified sandbox manifest and package state'
      rm -f "$manifest_path" "$state_dir/manifest.sha256"
    fi
    rm -f "$state_dir/removing"
    if [ "$1" = purge ]; then
      rm -rf "$state_dir"
    fi
    rmdir "$install_root/share" "$install_root" "$(dirname "$install_root")" 2>/dev/null || true
    ;;
  *) fail "unsupported postrm transition: ${1:-missing}" ;;
esac
