# Task-108 `.deb` Validation Handoff

Status: **deferred release-candidate manual validation**. Task-108 was accepted
as implementation-complete on 2026-08-18 after the Ubuntu 24.04 checkpoint.
The remaining rows and destructive fixtures in this runbook must be executed
once the full PRD implementation is assembled. They remain package
lifecycle/startup evidence, not task-005-b exact-renderer sandbox certification,
and are not recorded as passed by this deferral.

## Candidate

- Artifact: `/tmp/opencode/task-108-mainnet-completed/daedalus-11.2.0-86495-mainnet-dirty-x86_64-linux.deb`
- SHA-256: `32b843f3e3a36944c958a9cd795a6fcf3108c63e102d94282e5d06706a2e4f20`
- Base revision: `e92bf711512ca0d25186f3f06a0723c9895b709d`
- Build-relevant tracked/intend-to-add diff SHA-256: `d45898829f18d3ca9ca7349427081c58f0ae8d63d82e26fd7eb71700c843aee4`
- `flake.lock` SHA-256: `0c48b22f50e937ff7a2a92f88e96b2378db9d2894138473707eeb7b7818abf90`
- Matrix: `task-108-matrix-2026-08-18`

If any candidate byte changes, stop and obtain a new handoff. Do not install on
a host containing a real wallet, credentials, funds, or irreplaceable data.

## Runtime Driver

After installing the exact candidate, use the repository driver for the Ubuntu
24.04 bounded startup and running-process removal-refusal checkpoint instead of
executing that checkpoint manually:

```bash
.agent/plans/dapp-browser-cip30/research/task-108-ubuntu-24.04-deb-runtime-validation.sh
```

The driver prompts before the package-manager refusal test, preserves and checks
wallet-sentinel content and metadata, refuses to continue unless the exact
package Electron process is running, freezes that process before the test,
guarantees bounded process-group cleanup, keeps raw logs in a private
`/tmp/task-108-runtime-*` directory, and prints a normalized summary suitable
for returning to the Orchestrator. It intentionally refuses other matrix rows;
equivalent row-specific drivers or fixtures remain required there.

## Hosts

Use a clean snapshot for each row:

| Host | Expected state | AppArmor |
|---|---|---|
| Ubuntu 22.04.x | `wallet-only`, reason `apparmor-policy-proof-pending`, helper `0755` | no Daedalus profile |
| Ubuntu 24.04.x | `supported`, helper `4755` | exact-path default-allow/userns profile |
| Ubuntu 26.04.x | `supported`, helper `4755` | exact-path default-allow/userns profile |
| Debian 12.x | `supported`, helper `4755` | no Daedalus profile |
| Debian 13.x | `supported`, helper `4755` | no Daedalus profile |

An omitted distro check is also required: it must install wallet-only with
reason `unsupported-distro-version`, helper `0755`, and no profile.

## Per-Host Procedure

1. Restore a clean snapshot and copy the exact candidate to `/tmp/daedalus.deb`.
2. Record `cat /etc/os-release`, `uname -m`, `sha256sum /tmp/daedalus.deb`,
   `sha256sum flake.lock` from the source handoff, `dpkg-deb --info`, and
   `dpkg-deb --contents`. Keep raw host identifiers locally.
3. Create a non-sensitive sentinel outside the package tree:

   ```bash
   export XDG_DATA_HOME=/tmp/task-108-wallet-sentinel
   mkdir -p "$XDG_DATA_HOME/Daedalus"
   printf 'task-108-sentinel\n' >"$XDG_DATA_HOME/Daedalus/sentinel"
   sha256sum "$XDG_DATA_HOME/Daedalus/sentinel"
   ```

4. Trace and install with native package tools:

   ```bash
   sudo strace -ff -o /tmp/task-108-postinst.trace -e trace=%file \
     apt install -y /tmp/daedalus.deb
   version=$(dpkg-query -W -f='${Version}' daedalus-mainnet)
   sudo /var/lib/dpkg/info/daedalus-mainnet.postinst configure "$version"
   sudo /var/lib/dpkg/info/daedalus-mainnet.postinst configure "$version"
   ```

   `dpkg --configure daedalus-mainnet` correctly refuses when dpkg already marks
   the package configured. Repeated direct `postinst configure` invocations prove
   convergent maintainer-script behavior; interrupted recovery separately uses
   native `dpkg --configure -a` while the package is unconfigured.

5. Verify `/opt/daedalus/mainnet`, `/usr/bin/daedalus-mainnet`, desktop/icon,
   root ownership, non-writable ancestors, exact hashes, no unexpected
   SUID/SGID/capability, no `update-runner`, and absolute launcher config.
6. Verify the manifest with `jq` and compare the expected row above. Record
   `findmnt -no OPTIONS -T .../chrome-sandbox`, `stat`, `sha256sum`, and `getcap`.
7. On supported Ubuntu only, record `apparmor_parser --version`, non-loading
   parse, exact profile bytes/hash, the exact loaded profile line, reload, and
   reboot persistence. Do not disable AppArmor or alter global userns policy.
8. On Ubuntu 22.04, Debian, and the omitted host, prove that
   `/etc/apparmor.d/opt.daedalus.mainnet.electron` is absent and no matching
   profile is loaded.
9. Start only through `/opt/daedalus/mainnet/bin/daedalus` or the desktop entry
   under a bounded timeout. Record a sanitized success/failure category and
   confirm process argv contains no sandbox bypass. Do not open a dApp or remote
   URL.
10. Confirm the sentinel hash is unchanged. Inspect the host-local strace files
    for the sentinel path. Report `not accessed` only if no maintainer-script
    process or descendant accessed it; otherwise report only non-mutation.

## Lifecycle And Failure Cases

Run these from snapshots, preserving package-manager status and relevant hashes
before and after each operation:

- repeated configure;
- remove, purge, reinstall, and snapshot rollback;
- removal refusal while exact packaged Electron is running, followed by clean
  removal after exit;
- foreign/symlink/administrator-modified profile refusal on supported Ubuntu;
- conflicting `dpkg-statoverride` and `nosuid` refusal on a supported row;
- interrupted configure and successful `dpkg --configure -a` recovery;
- reboot persistence on supported Ubuntu.

For byte-distinct upgrade/downgrade fixtures, unpack a copy of the candidate
with `dpkg-deb -R`, decrement only the Debian `Version` and the embedded
`package_version` literal in all four maintainer scripts, normalize mtimes, and
rebuild with `dpkg-deb --root-owner-group -Zxz -z9 --build`. Record the fixture
SHA-256 and exact transformation. Test:

- old fixture to candidate upgrade;
- candidate to old fixture downgrade refusal with candidate state unchanged;
- the full mixed-script unwind, including `old-preinst abort-upgrade
  <new-version>` before and after candidate mutation.

For the mandatory failed-upgrade case, make a second recorded copy of the new
fixture whose `postinst configure` exits nonzero immediately before manifest
commit. Prove prior helper/profile/manifest/package state is restored exactly,
then install the untampered candidate and successfully reconfigure.

On one supported Ubuntu snapshot, induce a real profile parse or load failure
without weakening host policy. Configuration must fail nonzero with no retry,
prior state must remain exact, and the untampered candidate must subsequently
configure successfully.

## Evidence To Return

Return one normalized result per row containing:

- distro and point release, candidate SHA-256, package version, matrix row,
  support state/reason, and pass/fail;
- lifecycle case outcomes and package-manager final state;
- helper owner/mode/hash/capability/`nosuid` classifications;
- manifest and launcher classifications;
- AppArmor semantic/parse/load/reboot/cleanup results where required;
- startup-smoke category and no-bypass result;
- sentinel non-mutation and either `not accessed` or `inspection not proven`;
- failed-upgrade, negative-policy, and snapshot-rollback outcomes.

Do not return raw PIDs, usernames, hostnames, paths outside the tokenized package
roots, audit lines, full traces, environment, or stderr. Keep those host-local.

## Task-005-b Exact-Renderer Certification

Task-005-b rebuilt the mainnet candidate after the frozen probe correctly
rejected `libexec/electron` as an inherited symlink. The `.deb` builder now
removes that symlink before writing the wrapper and asserts the wrapper is a
regular root-owned mode-`0755` file.

- Candidate: `daedalus-11.2.0-86593-mainnet-dirty-x86_64-linux.deb`
- SHA-256: `7115d83f29c8ed21d8a8c7f8167816ab5107e2071046583487e51965be975fa7`
- Base revision: `18d523815ac98c9ad68bb5e1c4dff1a0f93a4515`
- Package build-relevant diff SHA-256: `4faf528ee370537367b43da739519b87593346d61aab2a2e76170640bf43a828`
- Probe SHA-256: `1f0f9188a68acb4c5c3676fb1163dcb1d8b3139dc5caeb8e4022875b9a8d281f`

The exact installed candidate passed on Ubuntu 24.04, Ubuntu 26.04, Debian 12,
and Debian 13. Ubuntu 22.04 rejected before renderer evidence with
`wallet-only-matrix-row:apparmor-policy-proof-pending`. Ubuntu AppArmor
unload failed closed; restoring the exact profile returned a passing probe.
Normalized records and immutable host-image identities are in the task-005-b evidence index.

These exact-renderer results do not complete the deferred task-807 package
lifecycle, reboot, destructive upgrade, or wallet-preservation matrix.
