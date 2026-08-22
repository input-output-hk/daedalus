# Task-109 Fedora 43 RPM Validation Handoff

Status: **task-109 production RPM implementation and Fedora 43 validation
passed on 2026-08-22.** The failed dedicated-domain Phase-A history remains
below as design provenance. Task-005-b still owns full installed-artifact
matrix certification and any later package-byte change requires revalidation.

## Production Resolution And Exact Evidence

The successful redesign does not maintain a bespoke Electron application
domain. The package-owned priority-200, cluster-specific CIL grants no
permissions. It assigns the exact Electron executable to Fedora `bin_t` and the
exact mode-`4755` helper to Fedora's stock `chrome_sandbox_exec_t`; Fedora's
reviewed `chrome_sandbox_t` policy remains available if Chromium selects the
SUID route. On the certified Fedora 43 row Chromium selected the user-namespace
route, so main and renderer SELinux process types remained `unconfined_t`.
Containment was proven independently on the exact renderer PID rather than
being inferred from that SELinux label.

Certified mainnet candidate:

- NEVRA: `daedalus-mainnet-0:11.2.0-0.git8462bbbaf.x86_64`
- RPM: `daedalus-11.2.0-0-mainnet-8462bbbaf-x86_64-linux.rpm`
- SHA-256: `09abf160f4f4fc1d51759c143987a321b5c92615d6534d03ce63e1845fc5c975`
- Fedora cloud base: `Fedora-Cloud-Base-Generic-43-1.6.x86_64.qcow2`
- Fedora image SHA-256: `846574c8a97cd2d8dc1f231062d73107cc85cbbbda56335e264a46e3a6c8ab2f`
- Kernel: `6.17.1-300.fc43.x86_64`
- SELinux: targeted policy, enforcing
- Electron/Chromium: `41.3.0` / `146.0.7680.188`

Observed package and runtime results:

- Native DNF install and replacement completed with the package `%pre`,
  `%post`, old `%preun` upgrade branch, and fixed `/opt/daedalus/mainnet`
  payload.
- The module was active at priority 200; `matchpathcon` and on-disk contexts
  matched `system_u:object_r:bin_t:s0` for Electron and
  `system_u:object_r:chrome_sandbox_exec_t:s0` for the helper.
- The root-owned helper was a regular non-symlink mode-`4755` file and every
  launcher/probe argv was free of sandbox-disabling switches.
- The package launcher reached Electron main initialization and trusted
  renderer creation under a bounded Xvfb smoke run. Preserving the launcher
  config as YAML fixed the prior `yamljs` parse failure caused by JSON
  reserialization.
- `webContents.getOSProcessId()` identified the exact Chromium renderer/zygote
  PID. `/proc` reported `NoNewPrivs: 1`, seccomp mode `2`, at least one seccomp
  filter, zero effective capabilities, separate PID and user namespaces, and
  separate UID/GID maps. Fedora's userns route shares the main mount namespace;
  this is recorded and is not misrepresented as a separate mount namespace.
- Package file hashes, modes, ownership, regular-file status, identity manifest,
  policy asset, renderer PID stability, and effective SELinux contexts all
  matched. The normalized probe result was `pass`.
- `ausearch -m AVC,USER_AVC -ts recent -c electron` returned no matches after
  package startup and both policy/containment probes.
- No remote URL, wallet, funds, credentials, hardware device, or user wallet
  state was used. The QEMU overlay and all disposable state were under
  `/tmp/daedalus-task109`.

The additive public outputs are
`rpm-installer-{mainnet,preprod,preview,selfnode}`. Hydra nonrequired jobs,
Buildkite builds, the `linux-rpm-package-contract` check, build/Nix docs, and
the canonical PRD/tracker use the same contract. Production dApp launch remains
disabled until task-005-b, task-103, and every later guest/release gate passes.

The private `rpmPrototype` output and its two source files were removed during
final cleanup after the production route superseded them. Their immutable
hashes and failed-checkpoint evidence remain below for provenance.

## Historical Failed Dedicated-Domain Checkpoint

## Candidate

- RPM: `/nix/store/pyfax9gdd0dbwjanj7c2dzkba6ljps9h-daedalus-mainnet-11.2.0-0.gitf302ed920.prototype8-rpm-prototype/daedalus-11.2.0-0-mainnet-f302ed920-x86_64-linux-prototype.rpm`
- SHA-256: `e1a0104f145e993b8d973e3e4aff37df2f33eeb4f6d6d57cf0d06e8554284b7a`
- NEVRA: `daedalus-mainnet-0:11.2.0-0.gitf302ed920.prototype8.x86_64`
- Base commit: `5200b45321524c517ac058e62d1dfef6bb866559`
- `flake.lock` SHA-256: `0c48b22f50e937ff7a2a92f88e96b2378db9d2894138473707eeb7b7818abf90`
- CIL SHA-256: `ada01a0b0fdc434faa1be19ec5b4652d2007f78509b84e91c3ecb65e1ff7b104`
- Probe SHA-256: `e87500c6e9992389d38826c97086eddbdc44e5ff8f1e42d06849558c0f4b168d`
- Module: `daedalus_mainnet`, priority `200`, prototype semantic version `1.0.4`
- Expected file contexts: `system_u:object_r:daedalus_mainnet_electron_exec_t:s0` and `system_u:object_r:daedalus_mainnet_sandbox_exec_t:s0`
- Expected default graphical-login source mapping: role `unconfined_r`, type `unconfined_t`
- Expected target: source SELinux user/range, role `unconfined_r`, type `daedalus_mainnet_t`

The dirty-worktree candidate records `sourceRevision: null`, `sourceDirty: true`,
and source prototype ID `f302ed920` in its manifest. The base commit and exact
file hashes above provide the remaining identity. Do not substitute a rebuild:
any byte change requires a new prototype revision, hash, and checkpoint.

## Observed Checkpoint

Fedora 43 ran under KVM with the default targeted policy in enforcing mode and
the default graphical login mapping `unconfined_r:unconfined_t`. Prototypes 3
through 8 were byte-distinct. Prototype 8 installed the enabled
`daedalus_mainnet` module at priority 200, recorded policy version `1.0.4`,
installed the helper as RPM-owned mode `4755`, and matched both packaged file
contexts. The transition fixture still failed before authoritative main and
renderer evidence, so bounded startup was not run. Its remaining normalized app
AVCs were:

- `daedalus_mainnet_t -> daedalus_mainnet_sandbox_exec_t`, class `file`, permission `execute`
- `daedalus_mainnet_t -> proc_t`, class `file`, permission `read`
- `daedalus_mainnet_t -> sysctl_fs_t`, class `dir`, permission `search`
- `daedalus_mainnet_t -> sysfs_t`, class `file`, permission `read`

The separate `systemd_coredump_t` read denial was diagnostic-only and was not
added to the application policy. Earlier byte-distinct runs established the
loader and Chromium initialization permissions now present in policy `1.0.4`;
the expanding runtime surface means further work requires a separately reviewed
Fedora desktop policy rather than tuple-by-tuple Phase A grants.

No sandbox bypass was used. The qcow2 overlay and all raw evidence were deleted
after the backing-image checksum matched, so snapshot rollback passed. This
failure does not authorize AVC-generated policy, permissive mode, `dontaudit`,
or Phase B lifecycle work.

The retained normalized result is:

```json
{
  "boundedStartup": "not-run",
  "candidateSha256": "e1a0104f145e993b8d973e3e4aff37df2f33eeb4f6d6d57cf0d06e8554284b7a",
  "fedoraVersion": "43",
  "install": "pass",
  "nevra": "daedalus-mainnet-0:11.2.0-0.gitf302ed920.prototype8.x86_64",
  "noSandboxBypass": true,
  "selinux": {
    "activeChecksumMatch": false,
    "avcCategory": "electron-runtime",
    "electronContextMatch": true,
    "extractedCilFingerprintMatch": false,
    "helperContextMatch": true,
    "localContextPrecedence": "none",
    "mainTransitionMatch": false,
    "moduleInstall": "pass",
    "modulePriority": "200",
    "moduleVersion": "1.0.4",
    "policyVersion": "selinux-policy-targeted-42.12-1.fc43.noarch",
    "rendererTransitionMatch": false,
    "sourceRole": "unconfined_r",
    "sourceType": "unconfined_t",
    "state": "enforcing",
    "toolVersions": [
      "policycoreutils-3.9-5.fc43.x86_64",
      "policycoreutils-python-utils-3.9-5.fc43.noarch"
    ]
  },
  "notes": "prototype 8 transition failed before authoritative main/renderer evidence",
  "snapshotRollback": "pass",
  "transitionFixture": "fail:electron-runtime"
}
```

Prototype 8 fixes the earlier module-basename, semantic-version, helper-mode,
and probe-enforcement defects. One session-side priority check raced and emitted
`missing`; the follow-up root diagnostic observed the enabled
`200 daedalus_mainnet` row in the same overlay. That correction does not change
the failed transition or authorize Phase B.

## Untested Evidence-Hardening Successor

The evidence-hardening source was built as prototype 9 before the final
handoff-only review corrections. It rejects disabled module rows, labels manifest policy
hash/version as configured rather than active observations, suppresses all
`--exit-only` output paths, pins handoff hashes, and adds the task-807 RPM gate.
It has no Fedora checkpoint evidence and must not replace prototype 8 in any
pass/fail claim.

- RPM: `/nix/store/8wz6jrv3ka7pvgbija6942965vqg3hn7-daedalus-mainnet-11.2.0-0.git246b06b0e.prototype9-rpm-prototype/daedalus-11.2.0-0-mainnet-246b06b0e-x86_64-linux-prototype.rpm`
- NEVRA: `daedalus-mainnet-0:11.2.0-0.git246b06b0e.prototype9.x86_64`
- SHA-256: `29920a5b1641942787244b47d46a5c0a42e5a65671d1c42669c7c2a4c540fb5e`
- `linux-rpm-prototype.nix`: `53209e8328b27119c21b55a1607ede00b28c39a8c0eb313671093ef26ecba348`
- `linux-rpm-prototype.cil`: `ada01a0b0fdc434faa1be19ec5b4652d2007f78509b84e91c3ecb65e1ff7b104`
- `x86_64-linux.nix`: `7f282a35e6f1495972f2fc86d75f25b035f93741157c45c6dbb0d19af2eed658`
- Probe: `cc52199ef4b46e57de5a6dcdc79dc0a405aa3e2241a3c32e018e4f3409164538`
- `flake.lock`: `0c48b22f50e937ff7a2a92f88e96b2378db9d2894138473707eeb7b7818abf90`

## Safety Boundary

Run only on a clean, snapshotted Fedora 43 x86_64 graphical desktop with the
default targeted SELinux policy in enforcing mode. Use a disposable user and
empty disposable `XDG_DATA_HOME`. Do not use a wallet, credentials, funds,
hardware device, dApp, remote URL, or real Daedalus data.

The prototype has only a `%post`; it intentionally has no production
upgrade/remove recovery machinery. A failed `%post` may leave RPM payload and
module state behind. Restore the VM snapshot after every outcome instead of
using this artifact to test uninstall or recovery.

The candidate is a dedicated enforcing domain with explicitly reviewed entry,
loader, cgroup, scheduling, user-namespace, and Electron/helper execution
permissions listed in `linux-rpm-prototype.cil`. It is not least-privilege
confinement, and startup may still fail closed before Electron JavaScript runs.
That is valid feasibility evidence, not permission to add a permissive domain,
`unconfined_domain_type`, `dontaudit`, an AVC-generated broad policy, or an
unsandboxed retry.

## Host-Local Run

Keep all command output in a mode-`0700` host-local directory. Substitute only
the transferred candidate and probe paths.

```bash
set -eu
umask 077
RAW="$HOME/task-109-raw"
RPM="$HOME/daedalus-11.2.0-0-mainnet-f302ed920-x86_64-linux-prototype.rpm"
PROBE="$HOME/main.cjs"
mkdir -m 0700 "$RAW"

test "$(uname -m)" = x86_64
test "$(getenforce)" = Enforcing
. /etc/os-release
test "$ID" = fedora
test "$VERSION_ID" = 43

printf '%s  %s\n%s  %s\n' \
  e1a0104f145e993b8d973e3e4aff37df2f33eeb4f6d6d57cf0d06e8554284b7a "$RPM" \
  e87500c6e9992389d38826c97086eddbdc44e5ff8f1e42d06849558c0f4b168d "$PROBE" \
  | sha256sum -c - | tee "$RAW/input-sha256"
rpm -K "$RPM" >"$RAW/rpm-signature" 2>&1 || true
rpm -qip "$RPM" >"$RAW/rpm-info"
rpm -qpR "$RPM" >"$RAW/rpm-requires"
rpm -qp --scripts "$RPM" >"$RAW/rpm-scripts"
id -Z >"$RAW/source-context"
getenforce >"$RAW/getenforce"
sestatus >"$RAW/sestatus"
semodule -lfull -m >"$RAW/modules-before"
semanage fcontext -l -C >"$RAW/local-fcontexts-before"

set +e
sudo dnf --disablerepo='*' install -y "$RPM" \
  >"$RAW/install.stdout" 2>"$RAW/install.stderr"
INSTALL_EXIT=$?
set -e
printf '%s\n' "$INSTALL_EXIT" >"$RAW/install.exit"

rpm -q daedalus-mainnet >"$RAW/installed-nevra" 2>&1 || true
semodule -lfull -m >"$RAW/modules-after" 2>&1 || true
matchpathcon /opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/electron \
  >"$RAW/electron-matchpathcon" 2>&1 || true
matchpathcon /opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/chrome-sandbox \
  >"$RAW/helper-matchpathcon" 2>&1 || true
stat -c '%C' /opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/electron \
  >"$RAW/electron-context" 2>&1 || true
stat -c '%C' /opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/chrome-sandbox \
  >"$RAW/helper-context" 2>&1 || true
sudo ausearch -m AVC,USER_AVC -ts recent >"$RAW/avc.raw" 2>&1 || true
```

If installation succeeds, extract the active module and run only the local
transition fixture:

```bash
set -eu
cd "$RAW"
sudo semodule -X 200 -c -E daedalus_mainnet
sudo chown "$(id -u):$(id -g)" daedalus_mainnet.cil
sha256sum daedalus_mainnet.cil >active-module-cil-sha256

PROFILE="$RAW/profile"
SOURCE_CONTEXT=$(id -Z)
set +e
timeout --signal=TERM --kill-after=5s 30s \
  env -u ELECTRON_DISABLE_SANDBOX \
    CHROME_DEVEL_SANDBOX=/opt/daedalus/mainnet/libexec/bundle-electron/lib/electron/chrome-sandbox \
    DAEDALUS_PROBE_DEBUG=1 \
    DAEDALUS_PROBE_PROFILE_ROOT="$PROFILE" \
    DAEDALUS_PROBE_MATRIX_REVISION=task-108-matrix-2026-08-18 \
    DAEDALUS_PROBE_MATRIX_ROW=fedora-43 \
    DAEDALUS_PROBE_SANDBOX_CLASS=combined-unattributed \
    DAEDALUS_PROBE_CLUSTER=mainnet \
    DAEDALUS_PROBE_SELINUX_SOURCE_CONTEXT="$SOURCE_CONTEXT" \
    /opt/daedalus/mainnet/libexec/electron "$PROBE" --transition-only \
    >"$RAW/transition.json" 2>"$RAW/transition.stderr"
TRANSITION_EXIT=$?
set -e
printf '%s\n' "$TRANSITION_EXIT" >"$RAW/transition.exit"
sudo ausearch -m AVC,USER_AVC -ts recent >"$RAW/avc-after-transition.raw" 2>&1 || true
```

Do not run the trusted-wallet startup unless installation and transition both
exit `0`, exact contexts match, and no unexplained AVC is present. If those
conditions hold, start `/opt/daedalus/mainnet/bin/daedalus` with a 30-second
external timeout and the disposable `XDG_DATA_HOME`; record only bounded startup
category and no-bypass result. Do not wait for synchronization.

## Return Evidence

Keep raw files, paths, PIDs, usernames, hostnames, AVC text, environment, and
stderr on the disposable host. Return this normalized record only:

```json
{
  "candidateSha256": "e1a0104f145e993b8d973e3e4aff37df2f33eeb4f6d6d57cf0d06e8554284b7a",
  "nevra": "daedalus-mainnet-0:11.2.0-0.gitf302ed920.prototype8.x86_64",
  "fedoraVersion": "43 or exact 43 point representation",
  "selinux": {
    "state": "enforcing",
    "policyVersion": "normalized version",
    "toolVersions": ["normalized versions"],
    "sourceRole": "role only",
    "sourceType": "type only",
    "moduleInstall": "pass|fail:<fixed-category>",
    "modulePriority": "200|missing",
    "moduleVersion": "1.0.4|unavailable",
    "activeChecksumMatch": true,
    "extractedCilFingerprintMatch": true,
    "localContextPrecedence": "none|present",
    "electronContextMatch": true,
    "helperContextMatch": true,
    "mainTransitionMatch": true,
    "rendererTransitionMatch": true,
    "avcCategory": "none|entry-transition|dynamic-loader|electron-runtime|renderer-create|backend-launch|user-state|other"
  },
  "install": "pass|fail:<fixed-category>",
  "transitionFixture": "pass|fail:<fixed-category>",
  "boundedStartup": "not-run|pass|timeout|signal|fail:<fixed-category>",
  "noSandboxBypass": true,
  "snapshotRollback": "pass|fail",
  "notes": "non-sensitive fixed-category clarification only"
}
```

Phase B remains forbidden until this evidence is reviewed. A failed transition
or unexplained AVC reopens the policy design; it does not authorize weakening
SELinux or changing Fedora 43 to supported by assertion.
