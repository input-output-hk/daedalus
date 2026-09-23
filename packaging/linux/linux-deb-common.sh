#!/bin/sh
set -eu

cluster='@CLUSTER@'
install_root='/opt/daedalus/@CLUSTER@'
state_dir='/var/lib/daedalus-package/@CLUSTER@'
profile_path='/etc/apparmor.d/opt.daedalus.@CLUSTER@.electron'
helper_path="$install_root/libexec/bundle-electron/lib/electron/chrome-sandbox"
manifest_path="$install_root/share/daedalus-sandbox-identity.json"
lock_dir="$state_dir/lifecycle.lock"
expected_helper_sha='@HELPER_SHA@'
package_version='@PACKAGE_VERSION@'
source_revision='@BUILD_REV@'
matrix_revision='task-108-matrix-2026-08-18'

fail() {
  printf '%s\n' "daedalus-${cluster}: $*" >&2
  exit 1
}

assert_manifest_value() {
  label=$1
  value=$2
  case "$value" in
    ''|*[!A-Za-z0-9._+~-]*) fail "unsafe manifest $label" ;;
  esac
}

lock() {
  mkdir -p "$state_dir"
  chmod 0700 "$state_dir"
  if ! mkdir "$lock_dir" 2>/dev/null; then
    fail 'another package lifecycle operation is active'
  fi
  trap 'rmdir "$lock_dir" 2>/dev/null || true' EXIT HUP INT TERM
}

read_host() {
  ID=''
  VERSION_ID=''
  if [ -r /etc/os-release ]; then
    # os-release is a distro-controlled data file containing shell assignments.
    # shellcheck disable=SC1091
    . /etc/os-release
  fi
  case "${ID:-}:${VERSION_ID:-}" in
    ubuntu:22.04*) matrix_row='ubuntu-22.04'; support_state='wallet-only'; reason='apparmor-policy-proof-pending'; policy_kind='none'; helper_mode='0755' ;;
    ubuntu:24.04*) matrix_row='ubuntu-24.04'; support_state='supported'; reason='supported'; policy_kind='apparmor'; helper_mode='4755' ;;
    ubuntu:26.04*) matrix_row='ubuntu-26.04'; support_state='supported'; reason='supported'; policy_kind='apparmor'; helper_mode='4755' ;;
    debian:12*) matrix_row='debian-12'; support_state='supported'; reason='supported'; policy_kind='none'; helper_mode='4755' ;;
    debian:13*) matrix_row='debian-13'; support_state='supported'; reason='supported'; policy_kind='none'; helper_mode='4755' ;;
    *) matrix_row='null'; support_state='wallet-only'; reason='unsupported-distro-version'; policy_kind='none'; helper_mode='0755' ;;
  esac
  host_id=${ID:-unknown}
  host_version=${VERSION_ID:-unknown}
  assert_manifest_value distribution-id "$host_id"
  assert_manifest_value distribution-version "$host_version"
}

assert_regular_file() {
  path=$1
  [ -f "$path" ] && [ ! -L "$path" ] || fail "unsafe or missing regular file: $path"
}

verify_helper() {
  assert_regular_file "$helper_path"
  [ "$(stat -c %h "$helper_path")" = 1 ] || fail 'chrome-sandbox must not be hard linked'
  [ "$(sha256sum "$helper_path" | cut -d' ' -f1)" = "$expected_helper_sha" ] || fail 'chrome-sandbox hash mismatch'
  path=$helper_path
  while [ "$path" != / ]; do
    path=$(dirname "$path")
    [ ! -L "$path" ] || fail "symlinked helper ancestor: $path"
    [ "$(stat -c %u:%g "$path")" = '0:0' ] || fail "non-root helper ancestor: $path"
    mode=$(stat -c %a "$path")
    [ $((0$mode & 0022)) -eq 0 ] || fail "writable helper ancestor: $path"
  done
  if command -v getcap >/dev/null 2>&1 && [ -n "$(getcap "$helper_path")" ]; then
    fail 'chrome-sandbox must not have file capabilities'
  fi
  unexpected_privileged=$(find "$install_root" -xdev -type f -perm /6000 ! -path "$helper_path" -print -quit)
  [ -z "$unexpected_privileged" ] || fail "unexpected privileged package file: $unexpected_privileged"
  if command -v getcap >/dev/null 2>&1; then
    unexpected_capability=$(getcap -r "$install_root" 2>/dev/null | grep -v "^$helper_path " | sed -n '1p')
    [ -z "$unexpected_capability" ] || fail 'unexpected package file capability'
  fi
  if dpkg-statoverride --list "$helper_path" >"$state_dir/statoverride" 2>/dev/null; then
    [ "$(cat "$state_dir/statoverride")" = "root root 4755 $helper_path" ] || fail 'conflicting chrome-sandbox statoverride'
  fi
}

snapshot_state() {
  rm -rf "$state_dir/previous.new"
  mkdir -p "$state_dir/previous.new"
  if [ -f "$profile_path" ] && [ ! -L "$profile_path" ]; then
    cp -p "$profile_path" "$state_dir/previous.new/profile"
  fi
  if [ -f "$manifest_path" ] && [ ! -L "$manifest_path" ]; then
    cp -p "$manifest_path" "$state_dir/previous.new/manifest"
  fi
  if [ -f "$helper_path" ] && [ ! -L "$helper_path" ]; then
    stat -c %a "$helper_path" >"$state_dir/previous.new/helper-mode"
  fi
  for state_file in profile.sha256 manifest.sha256 configured-version statoverride; do
    if [ -f "$state_dir/$state_file" ] && [ ! -L "$state_dir/$state_file" ]; then
      cp -p "$state_dir/$state_file" "$state_dir/previous.new/$state_file"
    fi
  done
  rm -rf "$state_dir/previous"
  mv "$state_dir/previous.new" "$state_dir/previous"
}

restore_state() {
  [ -d "$state_dir/previous" ] || return 0
  if [ -f "$state_dir/previous/helper-mode" ] && [ -f "$helper_path" ]; then
    chmod "$(cat "$state_dir/previous/helper-mode")" "$helper_path"
  elif [ -f "$helper_path" ]; then
    chmod 0755 "$helper_path"
  fi
  if [ -f "$state_dir/previous/profile" ]; then
    install -o root -g root -m 0644 "$state_dir/previous/profile" "$profile_path"
    command -v apparmor_parser >/dev/null 2>&1 || fail 'cannot restore AppArmor profile without apparmor_parser'
    apparmor_parser -r "$profile_path" || fail 'failed to restore prior AppArmor profile'
  elif [ -f "$state_dir/candidate-profile.sha256" ] && [ -f "$profile_path" ] && \
    [ "$(sha256sum "$profile_path" | cut -d' ' -f1)" = "$(cat "$state_dir/candidate-profile.sha256")" ]; then
    if command -v apparmor_parser >/dev/null 2>&1; then
      apparmor_parser -R "$profile_path" || fail 'failed to unload candidate AppArmor profile'
    fi
    rm -f "$profile_path"
  fi
  if [ -f "$state_dir/previous/manifest" ]; then
    install -o root -g root -m 0644 "$state_dir/previous/manifest" "$manifest_path"
  elif [ -f "$state_dir/manifest.sha256" ] && [ -f "$manifest_path" ] && \
    [ "$(sha256sum "$manifest_path" | cut -d' ' -f1)" = "$(cat "$state_dir/manifest.sha256")" ]; then
    rm -f "$manifest_path"
  fi
  for state_file in profile.sha256 manifest.sha256 configured-version statoverride; do
    if [ -f "$state_dir/previous/$state_file" ]; then
      cp -p "$state_dir/previous/$state_file" "$state_dir/$state_file"
    else
      rm -f "$state_dir/$state_file"
    fi
  done
  rm -f "$state_dir/candidate-profile.sha256"
}

write_manifest() {
  policy_identity=$1
  tmp="$manifest_path.new.$$"
  launcher_sha=$(sha256sum "$install_root/bin/daedalus" | cut -d' ' -f1)
  frontend_sha=$(sha256sum "$install_root/libexec/daedalus-frontend" | cut -d' ' -f1)
  wrapper_sha=$(sha256sum "$install_root/libexec/electron" | cut -d' ' -f1)
  electron_sha=$(sha256sum "$install_root/libexec/bundle-electron/lib/electron/electron" | cut -d' ' -f1)
  policy_files=''
  if [ "$policy_kind" = apparmor ]; then
    policy_sha=$(sha256sum "$profile_path" | cut -d' ' -f1)
    policy_files=$(printf ',"policyAsset":{"sha256":"%s"}' "$policy_sha")
  fi
  umask 077
  printf '{"schemaVersion":2,"packageFamily":"deb","matrixRevision":"%s","matrixRow":%s,"distribution":{"id":"%s","versionId":"%s"},"supportState":"%s","reason":"%s","cluster":"%s","packageVersion":"%s","sourceRevision":"%s","policy":{"kind":"%s","semanticIdentity":"%s","processLabel":"%s/libexec/bundle-electron/lib/electron/electron","loadedProfileSuffix":" (unconfined)","requiredAbi":"4.0","requiredFlags":["default_allow"],"requiredRules":["userns"]},"helper":{"mode":"%s","sha256":"%s"},"launch":{"launcher":"%s/bin/daedalus","electron":"%s/libexec/bundle-electron/lib/electron/electron"},"files":{"launcher":{"sha256":"%s"},"frontend":{"sha256":"%s"},"wrapper":{"sha256":"%s"},"electron":{"sha256":"%s"},"chromeSandbox":{"sha256":"%s"}%s}}\n' \
    "$matrix_revision" \
    "$(if [ "$matrix_row" = null ]; then printf null; else printf '"%s"' "$matrix_row"; fi)" \
    "$host_id" "$host_version" "$support_state" "$reason" "$cluster" "$package_version" "$source_revision" \
    "$policy_kind" "$policy_identity" "$install_root" "$helper_mode" "$expected_helper_sha" "$install_root" "$install_root" \
    "$launcher_sha" "$frontend_sha" "$wrapper_sha" "$electron_sha" "$expected_helper_sha" "$policy_files" >"$tmp"
  chown root:root "$tmp"
  chmod 0644 "$tmp"
  mv "$tmp" "$manifest_path"
  sha256sum "$manifest_path" | cut -d' ' -f1 >"$state_dir/manifest.sha256"
  chmod 0600 "$state_dir/manifest.sha256"
}

install_apparmor_profile() {
  template="$install_root/share/apparmor/$matrix_row"
  assert_regular_file "$template"
  command -v apparmor_parser >/dev/null 2>&1 || fail 'supported Ubuntu row requires apparmor_parser'
  apparmor_parser --skip-kernel-load "$template" || fail 'AppArmor profile semantic parse failed'
  tmp="$profile_path.new.$$"
  install -o root -g root -m 0600 "$template" "$tmp"
  if [ -e "$profile_path" ]; then
    assert_regular_file "$profile_path"
    if [ -f "$state_dir/profile.sha256" ]; then
      [ "$(sha256sum "$profile_path" | cut -d' ' -f1)" = "$(cat "$state_dir/profile.sha256")" ] || fail 'refusing to replace modified AppArmor profile'
    else
      rm -f "$tmp"
      fail 'refusing to replace foreign AppArmor profile'
    fi
  fi
  chmod 0644 "$tmp"
  sha256sum "$tmp" | cut -d' ' -f1 >"$state_dir/candidate-profile.sha256"
  chmod 0600 "$state_dir/candidate-profile.sha256"
  mv "$tmp" "$profile_path"
  if ! apparmor_parser -r "$profile_path"; then
    restore_state
    fail 'AppArmor profile load failed and prior state was restored'
  fi
  expected_label="$install_root/libexec/bundle-electron/lib/electron/electron (unconfined)"
  grep -Fx "$expected_label" /sys/kernel/security/apparmor/profiles >/dev/null || {
    restore_state
    fail 'AppArmor profile loaded identity mismatch'
  }
  sha256sum "$profile_path" | cut -d' ' -f1 >"$state_dir/profile.sha256"
  chmod 0600 "$state_dir/profile.sha256"
}
