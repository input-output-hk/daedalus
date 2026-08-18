#!/bin/sh
# shellcheck source=linux-deb-common.sh
. "$(dirname "$0")/linux-deb-common.sh"

configure_package() (
    set -eu
    read_host
    verify_helper
    if [ "$support_state" = supported ]; then
      if command -v findmnt >/dev/null 2>&1 && findmnt -no OPTIONS -T "$helper_path" | tr ',' '\n' | grep -Fx nosuid >/dev/null; then
        fail 'supported row cannot establish SUID helper on a nosuid mount'
      fi
      chown root:root "$helper_path"
      chmod 4755 "$helper_path"
    else
      chown root:root "$helper_path"
      chmod 0755 "$helper_path"
    fi
    policy_identity=none
    if [ "$policy_kind" = apparmor ]; then
      install_apparmor_profile
      policy_identity="default-allow-userns-$matrix_row"
    fi
    write_manifest "$policy_identity"
    printf '%s\n' "$package_version" >"$state_dir/configured-version"
    chmod 0600 "$state_dir/configured-version"
    rm -f "$state_dir/candidate-profile.sha256"
    rm -rf "$state_dir/previous"
)

configure_transaction() (
  set +e
  configure_package
  configure_status=$?
  if [ "$configure_status" -ne 0 ]; then
    set -e
    restore_state
    exit "$configure_status"
  fi
)

lock
case "${1:-}" in
  configure)
    set +e
    configure_transaction
    configure_status=$?
    set -e
    if [ "$configure_status" -ne 0 ]; then
      fail 'configuration failed and prior state was restored'
    fi
    ;;
  abort-remove)
    rm -f "$state_dir/removing"
    ;;
  abort-upgrade|abort-deconfigure)
    restore_state
    ;;
  *) fail "unsupported postinst transition: ${1:-missing}" ;;
esac
