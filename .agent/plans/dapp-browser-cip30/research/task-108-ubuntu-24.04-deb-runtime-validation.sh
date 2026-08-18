#!/usr/bin/env bash

set -Eeuo pipefail

package=daedalus-mainnet
cluster=mainnet
startup_timeout=60
assume_yes=false

usage() {
  cat <<'EOF'
Usage: .agent/plans/dapp-browser-cip30/research/task-108-ubuntu-24.04-deb-runtime-validation.sh [--yes] [--startup-timeout SECONDS]

Runs the task-108 bounded startup and running-process package-removal refusal
checks against an installed daedalus-mainnet .deb on Ubuntu 24.04. The attempted
package removal must fail while Electron is frozen in place. Raw logs remain in
a private /tmp directory.
EOF
}

while (($#)); do
  case "$1" in
    --yes)
      assume_yes=true
      shift
      ;;
    --startup-timeout)
      [[ $# -ge 2 ]] || { usage >&2; exit 2; }
      startup_timeout=$2
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      usage >&2
      exit 2
      ;;
  esac
done

[[ "$startup_timeout" =~ ^[1-9][0-9]*$ ]] || {
  printf 'startup timeout must be a positive integer\n' >&2
  exit 2
}
[[ $EUID -ne 0 ]] || {
  printf 'run this script as the desktop user, not root\n' >&2
  exit 2
}
: "${XDG_DATA_HOME:?set XDG_DATA_HOME to the task-108 wallet-sentinel root}"
[[ -z ${ELECTRON_DISABLE_SANDBOX:-} ]] || {
  printf 'unset ELECTRON_DISABLE_SANDBOX before validation\n' >&2
  exit 2
}

root="/opt/daedalus/$cluster"
launcher="$root/bin/daedalus"
electron="$root/libexec/bundle-electron/lib/electron/electron"
helper="$root/libexec/bundle-electron/lib/electron/chrome-sandbox"
profile="/etc/apparmor.d/opt.daedalus.$cluster.electron"
manifest="$root/share/daedalus-sandbox-identity.json"
state_dir="/var/lib/daedalus-package/$cluster"
sentinel="$XDG_DATA_HOME/Daedalus/sentinel"
timestamp=$(date -u +%Y%m%dT%H%M%SZ)
evidence_dir="/tmp/task-108-runtime-$timestamp"

for command in apparmor_parser dpkg-query grep jq pgrep ps readlink setsid sha256sum sudo timeout; do
  command -v "$command" >/dev/null 2>&1 || {
    printf 'missing required command: %s\n' "$command" >&2
    exit 2
  }
done

[[ -x "$launcher" && -x "$electron" && -f "$helper" && -f "$profile" && -f "$manifest" ]] || {
  printf 'the expected %s system package is not installed\n' "$package" >&2
  exit 2
}
[[ -f "$sentinel" && ! -L "$sentinel" ]] || {
  printf 'missing regular wallet sentinel: %s\n' "$sentinel" >&2
  exit 2
}
[[ $(dpkg-query -W -f='${db:Status-Status}' "$package" 2>/dev/null) == installed ]] || {
  printf '%s is not configured\n' "$package" >&2
  exit 2
}
jq -e '
  .matrixRevision == "task-108-matrix-2026-08-18"
  and .matrixRow == "ubuntu-24.04"
  and .supportState == "supported"
  and .policy.loadedProfileSuffix == " (unconfined)"
' "$manifest" >/dev/null || {
  printf 'installed manifest does not match the task-108 Ubuntu 24.04 contract\n' >&2
  exit 2
}

find_package_electron_pids() {
  local proc_exe pid resolved
  for proc_exe in /proc/[0-9]*/exe; do
    [[ -e "$proc_exe" ]] || continue
    resolved=$(readlink "$proc_exe" 2>/dev/null || true)
    [[ "$resolved" == "$electron" ]] || continue
    pid=${proc_exe#/proc/}
    printf '%s\n' "${pid%/exe}"
  done
}

find_session_electron_pids() {
  local pgid pid
  while IFS= read -r pid; do
    [[ -n "$pid" ]] || continue
    pgid=$(ps -o pgid= -p "$pid" 2>/dev/null || true)
    pgid=${pgid//[[:space:]]/}
    [[ "$pgid" == "$session_pid" ]] || continue
    printf '%s\n' "$pid"
  done < <(find_package_electron_pids)
}

mapfile -t existing_pids < <(find_package_electron_pids)
((${#existing_pids[@]} == 0)) || {
  printf 'close the existing package Electron processes first (count=%s)\n' "${#existing_pids[@]}" >&2
  exit 2
}

printf '%s\n' \
  'This validation launches Daedalus temporarily and attempts to remove its package.' \
  'The removal is expected to fail while Electron is running; the package must remain installed.' >&2
if [[ "$assume_yes" != true ]]; then
  read -r -p 'Continue? [y/N] ' response
  [[ "$response" == y || "$response" == Y ]] || exit 0
fi

umask 077
mkdir "$evidence_dir"
stdout_log="$evidence_dir/startup.stdout"
stderr_log="$evidence_dir/startup.stderr"
process_log="$evidence_dir/processes.txt"
remove_log="$evidence_dir/remove.txt"
summary="$evidence_dir/summary.txt"
before_hashes="$evidence_dir/before.sha256"
after_hashes="$evidence_dir/after.sha256"
sentinel_before="$evidence_dir/sentinel-before.txt"
sentinel_after="$evidence_dir/sentinel-after.txt"
session_pid=
apt_guard_pid=
cleaned=false
failures=0

record_failure() {
  printf 'FAIL: %s\n' "$*" | tee -a "$summary" >&2
  failures=$((failures + 1))
}

record_sentinel_identity() {
  local output=$1
  sha256sum "$sentinel" >"$output"
  stat -c 'uid=%u gid=%g mode=%a links=%h type=%F size=%s mtime=%Y' "$sentinel" >>"$output"
}

check_sentinel_identity() {
  record_sentinel_identity "$sentinel_after"
  if ! diff -u "$sentinel_before" "$sentinel_after" >"$evidence_dir/sentinel.diff"; then
    record_failure 'wallet-sentinel content or metadata changed'
  fi
}

cleanup() {
  [[ "$cleaned" == false ]] || return 0
  trap '' INT TERM
  if [[ -n "$apt_guard_pid" ]]; then
    kill -TERM -- "-$apt_guard_pid" 2>/dev/null || kill -TERM "$apt_guard_pid" 2>/dev/null || true
    sleep 1
    kill -KILL -- "-$apt_guard_pid" 2>/dev/null || kill -KILL "$apt_guard_pid" 2>/dev/null || true
    wait "$apt_guard_pid" 2>/dev/null || true
  fi
  if [[ -n "$session_pid" ]]; then
    kill -CONT -- "-$session_pid" 2>/dev/null || true
    kill -TERM -- "-$session_pid" 2>/dev/null || true
  fi
  sleep 3
  if [[ -n "$session_pid" ]]; then
    kill -KILL -- "-$session_pid" 2>/dev/null || true
    wait "$session_pid" 2>/dev/null || true
  fi
  cleaned=true
}

abort() {
  cleanup
  exit 130
}

trap cleanup EXIT
trap abort INT TERM

sudo -v
sha256sum "$helper" "$profile" "$manifest" "$sentinel" >"$before_hashes"
record_sentinel_identity "$sentinel_before"

setsid env XDG_DATA_HOME="$XDG_DATA_HOME" "$launcher" >"$stdout_log" 2>"$stderr_log" &
session_pid=$!

electron_found=false
for ((second = 0; second < startup_timeout; second++)); do
  mapfile -t electron_pids < <(find_session_electron_pids)
  if ((${#electron_pids[@]} > 0)); then
    electron_found=true
    break
  fi
  kill -0 "$session_pid" 2>/dev/null || break
  sleep 1
done

if [[ "$electron_found" != true ]]; then
  record_failure "package Electron did not start within $startup_timeout seconds"
  cleanup
  check_sentinel_identity
  trap - EXIT INT TERM
  printf 'startup_stdout_bytes=%s\n' "$(wc -c <"$stdout_log")" | tee -a "$summary"
  printf 'startup_stderr_bytes=%s\n' "$(wc -c <"$stderr_log")" | tee -a "$summary"
  printf 'raw_evidence_saved=true\nresult=FAIL failures=%s\n' "$failures" | tee -a "$summary" >&2
  exit 1
else
  printf 'electron_started=true\n' | tee -a "$summary"
fi

: >"$process_log"
mapfile -t group_pids < <(pgrep -g "$session_pid" 2>/dev/null || true)
for pid in "${group_pids[@]}"; do
  [[ -r "/proc/$pid/cmdline" ]] || continue
  argv=$(tr '\0' ' ' <"/proc/$pid/cmdline" 2>/dev/null || true)
  printf 'pid=%s argv=%s\n' "$pid" "$argv" >>"$process_log"
  case "$argv" in
    *--no-sandbox*|*--disable-setuid-sandbox*|*ELECTRON_DISABLE_SANDBOX*)
      record_failure 'sandbox-bypass argument found'
      ;;
  esac
done

mapfile -t electron_pids < <(find_session_electron_pids)
labels_checked=0
labels_matching=0
for pid in "${electron_pids[@]}"; do
  label=$(sudo cat "/proc/$pid/attr/current" 2>/dev/null || true)
  labels_checked=$((labels_checked + 1))
  if [[ "$label" == "$electron (unconfined)" ]]; then
    labels_matching=$((labels_matching + 1))
  else
    record_failure 'unexpected Electron label'
  fi
done
printf 'electron_labels_checked=%s\nelectron_labels_matching=%s\n' "$labels_checked" "$labels_matching" \
  | tee -a "$summary"
if ((failures == 0)); then
  printf 'sandbox_bypass_arguments=absent\n' | tee -a "$summary"
fi

mapfile -t electron_pids < <(find_session_electron_pids)
if ((${#electron_pids[@]} == 0)); then
  record_failure 'package Electron exited before the removal-refusal test'
  cleanup
  check_sentinel_identity
  trap - EXIT INT TERM
  printf 'raw_evidence_saved=true\nresult=FAIL failures=%s\n' "$failures" | tee -a "$summary" >&2
  exit 1
fi

# A stopped exact Electron process cannot exit between this check and prerm.
if ! kill -STOP -- "-$session_pid" 2>/dev/null; then
  record_failure 'package process group exited before it could be frozen'
  cleanup
  check_sentinel_identity
  trap - EXIT INT TERM
  printf 'raw_evidence_saved=true\nresult=FAIL failures=%s\n' "$failures" | tee -a "$summary" >&2
  exit 1
fi
sleep 1
mapfile -t electron_pids < <(find_session_electron_pids)
if ((${#electron_pids[@]} == 0)); then
  record_failure 'frozen package Electron precondition was lost'
  cleanup
  check_sentinel_identity
  trap - EXIT INT TERM
  printf 'raw_evidence_saved=true\nresult=FAIL failures=%s\n' "$failures" | tee -a "$summary" >&2
  exit 1
fi
printf 'electron_frozen=true\n' | tee -a "$summary"

remove_status=0
set +e
timeout --kill-after=10s 120s sudo -n apt remove -y "$package" >"$remove_log" 2>&1 &
apt_guard_pid=$!
wait "$apt_guard_pid"
remove_status=$?
apt_guard_pid=
set -e
printf 'remove_status=%s\n' "$remove_status" | tee -a "$summary"
((remove_status != 0)) || record_failure 'package removal unexpectedly succeeded'
[[ "$remove_status" != 124 && "$remove_status" != 137 ]] \
  || record_failure 'package removal timed out'
grep -Fq 'a password is required' "$remove_log" \
  && record_failure 'cached sudo credential was unavailable to the package-manager test'
grep -Fq 'close Daedalus before removing the package' "$remove_log" \
  || record_failure 'package removal did not report the running-process refusal'

cleanup
trap - EXIT INT TERM

package_installed=false
if [[ $(dpkg-query -W -f='${db:Status-Status}' "$package" 2>/dev/null || true) == installed ]]; then
  package_installed=true
else
  record_failure 'package is not installed after removal refusal'
fi
[[ ! -e "$state_dir/removing" ]] || record_failure 'removal marker remains after abort-remove'

if [[ "$package_installed" == true ]]; then
  set +e
  verify_output=$(sudo dpkg --verify "$package" 2>&1)
  verify_status=$?
  set -e
  printf '%s' "$verify_output" >"$evidence_dir/dpkg-verify.txt"
  ((verify_status == 0)) || record_failure 'dpkg verification failed after removal refusal'
  [[ ! -s "$evidence_dir/dpkg-verify.txt" ]] || record_failure 'dpkg verification reported changed files'
fi

loaded_label=$(sudo grep -F "$electron" /sys/kernel/security/apparmor/profiles 2>/dev/null || true)
[[ "$loaded_label" == "$electron (unconfined)" ]] \
  || record_failure 'AppArmor profile identity changed after removal refusal'

if [[ -f "$helper" && -f "$profile" && -f "$manifest" && -f "$sentinel" ]]; then
  sha256sum "$helper" "$profile" "$manifest" "$sentinel" >"$after_hashes"
else
  : >"$after_hashes"
  record_failure 'package or wallet-sentinel files are missing after removal refusal'
fi
if ! diff -u "$before_hashes" "$after_hashes" >"$evidence_dir/hashes.diff"; then
  record_failure 'package or wallet-sentinel hashes changed'
fi
check_sentinel_identity

mapfile -t remaining_pids < <(find_package_electron_pids)
((${#remaining_pids[@]} == 0)) || record_failure 'package Electron processes remain after cleanup'

printf 'startup_stdout_bytes=%s\n' "$(wc -c <"$stdout_log")" | tee -a "$summary"
printf 'startup_stderr_bytes=%s\n' "$(wc -c <"$stderr_log")" | tee -a "$summary"
printf 'raw_evidence_saved=true\n' | tee -a "$summary"

if ((failures > 0)); then
  printf 'result=FAIL failures=%s\n' "$failures" | tee -a "$summary" >&2
  exit 1
fi

printf 'package_status=installed\nremoval_marker=absent\nprofile_identity=pass\nhashes_unchanged=true\nresult=PASS\n' \
  | tee -a "$summary"
