# Linux Chromium Sandbox Packaging Evidence

Status: **portable strategy rejected.** This note retains negative evidence from
the home-directory self-extracting `.bin` spike. The accepted Linux product
strategy is **system `.deb` and `.rpm` packages**; see
[06-linux-system-package-decision.md](./06-linux-system-package-decision.md).
Task-005 acceptance still requires positive exact-renderer OS sandbox proof on
installed `.deb`/`.rpm` artifacts. Production dApp launch remains disabled.

## Current Task State

- Task status: `in_progress`; task-005 acceptance is not met until `.deb`/`.rpm`
  packaged sandbox proof exists.
- Product decision (2026-08-12): ship Linux as `.deb` and `.rpm` only; reject
  portable `.bin`, AppImage, Flatpak, Snap, and other Linux channels. Durable
  record: research `06`.
- Portable spike status: package-equivalent Ubuntu 24.04 home install cannot
  start Chromium sandbox bootstrap (`SIGILL`). A local-only unsandboxed control
  isolated failure to sandbox startup. That evidence **supports rejecting
  portable packaging**; it is not proof that system packages work until retested.
- Production status: both production Linux launch sites still contain sandbox
  bypasses; remote dApp launch remains disabled on every platform.
- Cross-platform boundary: this note covers Linux only. Windows and macOS remain
  tasks 802 and 807.

## Decision (superseded candidate → accepted replacement)

**Rejected candidate:** unprivileged-user-namespace-only path for the
home-directory self-extracting installer. That installer cannot establish a
root-owned mode-`4755` helper; Ubuntu 24.04 AppArmor userns policy is
path-sensitive; the portable proof variant failed sandbox bootstrap.

**Accepted strategy:** privileged system packages (`.deb` and `.rpm`) installing
to a fixed `/opt/...` path with postinst SUID `chrome-sandbox` and/or AppArmor
`userns` profile, no process-wide sandbox disablement, fail-closed dApp canary.
Full decision text, rejections, ownership, and migration notes:
[06-linux-system-package-decision.md](./06-linux-system-package-decision.md).

Automatically adding `--no-sandbox`, retrying unsandboxed, changing host policy
from the app, or weakening renderer assertions remains forbidden.

Task-103 owns removal of the two current launch bypasses and the
pre-remote-content runtime canary against the system package. Task-108/109 own
`.deb`/`.rpm` implementation; task-110 owns `.bin` retirement and auto-update
migration. Task-107, task-802, task-807, and task-903-a own later real-guest and
release-candidate proof.

## Verified Baseline

- `source/main/webpack.config.js` passes `--disable-setuid-sandbox` and
  `--no-sandbox` to development Electron.
- `nix/internal/x86_64-linux.nix` passes both switches from the packaged
  `daedalus-frontend` wrapper.
- `nix/internal/linux-self-extracting-archive.sh` replaces the existing
  `$HOME/.daedalus/<cluster>` tree and extracts the package as the current user.
- The package embeds and repairs Electron's ELF interpreter and launches the
  installed binary through `libexec/electron`; a Nix-store or `node_modules`
  Electron launch is not package evidence.
- Electron documents that `--no-sandbox` disables Chromium's sandbox for all
  processes and is testing-only. Enabling Node integration also disables a
  renderer's sandbox.
- Ubuntu documents that AppArmor can deny unprivileged user namespaces to
  unprofiled applications or applications installed at an unexpected path.
  A home-installed executable must therefore be tested on every supported
  Ubuntu version rather than inferred from a kernel sysctl alone.

Sources consulted on 2026-08-11:

- <https://www.electronjs.org/docs/latest/tutorial/sandbox>
- <https://www.electronjs.org/docs/latest/api/web-contents#contentsgetosprocessid>
- <https://ubuntu.com/blog/ubuntu-23-10-restricted-unprivileged-user-namespaces>
- `.agent/plans/dapp-browser-cip30/research/01-hostile-renderer-threat-model-traceability.md`
- Local Ubuntu profiles `/etc/apparmor.d/unprivileged_userns` and
  `/etc/apparmor.d/chrome` were inspected. They confirm that Ubuntu treats
  userns permission as profile-sensitive, but they do not prove which operation
  caused this Electron trap.

## Execution And Review Provenance

- The user authorized all safe automated testing available on the Ubuntu 24.04
  development host. No authorization was given or needed to alter host-wide
  AppArmor/sysctl policy; no such alteration was performed.
- Required Daedalus docs and Electron, Nix, build, test, and docs-update
  workflows are recorded in the canonical plan at
  `../task-plans/task-005.md`. The planning transcript is
  `../task-plans/task-005-plan-review.md`; the automated run, first code review,
  fixes, evidence, and current decision handoff are preserved in
  `../task-plans/task-005-impl-review.md`.
- The first implementation review required changes for unbounded startup/load
  and cleanup, incomplete evidence redaction, overclaiming the `SIGILL` cause,
  the incorrect `--type=renderer` argv assumption, absent passing evidence, and
  the unresolved packaging/matrix decision. Agent-executable code and wording
  defects are fixed; acceptance/product blockers remain.
- Raw stderr, traces, debugger output, temporary profiles, installed proof
  files, and unsanitized paths remain only in the disposable local evidence
  area under `/tmp/opencode`. They are not committed research and must not be
  transferred. This document retains only normalized results and immutable
  hashes.
- No task completion, PRD completion pointer, tracker completion, release gate,
  or final task commit is claimed.

## Probe Contract

`scripts/linux-chromium-sandbox-probe/main.cjs` is a local-only package
surrogate. It creates one hidden renderer with no preload, IPC, Node integration,
remote content, persistent session, or application code. Main identifies the
exact renderer through `webContents.getOSProcessId()` and reads that PID's
allowlisted Linux `/proc` evidence.

A positive result requires all of the following from the same renderer PID:

- the PID comes from Electron's documented `webContents.getOSProcessId()` API;
- command line records Chromium's process type and no known sandbox bypass. On
  this Electron 41 Linux run, the associated renderer OS PID retained
  `--type=zygote`; therefore exact Electron PID association is authoritative and
  the probe accepts recorded `--type=renderer` or `--type=zygote` rather than
  falsely requiring argv alone to identify renderer ownership;
- `NoNewPrivs: 1`;
- `Seccomp: 2` and a positive `Seccomp_filters` count when the kernel exposes it;
- zero `CapEff`;
- user, PID, and mount namespaces differ from the Electron main process;
- user/group namespace maps differ from the Electron main process;
- installed Electron, wrapper, helper, and probe hashes and modes are recorded.
- the installed helper is not both root-owned and setuid.

The probe refuses `ELECTRON_DISABLE_SANDBOX`, refuses an Electron executable
outside an installed `libexec` tree, emits deterministic JSON, removes its
temporary profile, and exits nonzero on an assertion failure or timeout.
`process.sandboxed` is intentionally absent: without a preload or Node-enabled
renderer it is not a trustworthy page-side assertion, and same-PID OS evidence
is stronger.

The in-script hard deadline starts after Electron loads the main script and
before `app.whenReady()`. It covers readiness, local-page load, renderer
evidence, and a two-second bounded session cleanup. It cannot cover a native
failure before the script loads, as demonstrated by the current `SIGILL`.
Operator commands therefore also wrap the entire installed Electron invocation
with a 30-second external deadline and five-second forced-termination grace.
Debug mode emits only fixed stage names and the filtered Chromium process-type
argument; it emits no path or arbitrary application data.

The historical `noUsableSetuidHelper` assertion applied only to the rejected
portable candidate. For `.deb`/`.rpm` proof, the probe must validate the approved
root-owned helper contract when userns are unavailable (mode `4755`, root-owned)
and must record helper identity when userns are used without SUID.

Run the dependency-free redaction golden test before host execution:

```bash
node scripts/linux-chromium-sandbox-probe/main.cjs --self-test
node --check scripts/linux-chromium-sandbox-probe/main.cjs
```

## Evidence Privacy

Raw argv, command lines, environment, paths, and stderr remain in a mode-`0700`
directory on the disposable host and must not be committed or transferred. The
exported records use the following deterministic substitutions, longest root
first:

| Host-local root | Export token |
|---|---|
| Installed package root | `<INSTALL_ROOT>` |
| Probe directory | `<PROBE_ROOT>` |
| Temporary Electron profile | `<PROFILE_ROOT>` |
| Disposable home | `<HOME>` |

Argv remains an ordered JSON string array. Stderr export contains only exit
code, byte count, SHA-256 of raw bytes, one fixed category, a sanitized UTF-8
excerpt of at most 8192 bytes, and a truncation marker when applicable. The
sanitizer applies component-boundary root replacement longest-first; removes
all URI schemes; removes usernames and hostnames case-insensitively; removes
sensitive environment-derived values and assignment values; replaces remaining
absolute paths across punctuation delimiters; and rejects export if prohibited
content remains. It retains only fixed probe/fatal/sandbox/AppArmor/userns lines
from stderr and substitutes `<NON_PROBE_STDERR_REDACTED>` when raw stderr has no
relevant line. Raw-byte count and hash still preserve correlation. Golden tests
cover braces/brackets, `file://`, mixed-case identity data, values outside an
assignment, root tokens, residual rejection, forbidden switches, and timeout.
Do not include a token reverse map in returned evidence.

## Required Inputs

Packaging decision is complete (`system_package` via `.deb`/`.rpm`; see research
`06`). Before task-005 completion, release/product engineering must still
provide:

1. The authoritative supported x86_64 Linux distribution/version matrix for
   `.deb` and `.rpm` rows, or the accountable owner and date by which it will be
   supplied.
2. One disposable default-policy host for every enabled matrix row.
3. A snapshotted disposable host where the selected sandbox prerequisite can
   safely be denied, including rollback access.
4. Exact `.deb` and `.rpm` proof artifacts from task-108/109 (or package-
   equivalent proof builds). The portable `.bin` proof artifact below is
   diagnostic only and is not a release candidate.
5. A host-local Node.js runtime for the sanitizer (evidence tool only).

No wallet profile, credentials, funds, Cardano network, hardware wallet, dApp,
or remote URL is needed. Prefer disposable VMs; do not install proof packages
over a real wallet-bearing system without a snapshot.

## Package-Equivalent Proof Variant

Task-005 does not remove production bypasses. Build the proof variant in an
isolated copy of the reviewed task workspace, not in a wallet-bearing checkout:

1. Record the base revision with `git rev-parse HEAD`. If build-relevant task
   changes are applied in the isolated source, record their complete binary diff
   hash separately; the executed Ubuntu artifact used base `HEAD` plus only the
   two-file proof patch, with the probe supplied externally and hashed.
2. In the isolated copy only, remove `--disable-setuid-sandbox` and
   `--no-sandbox` from the `ManageElectronProcessPlugin` argv in
   `source/main/webpack.config.js` and from the `daedalus-frontend` exec line in
   `nix/internal/x86_64-linux.nix`.
3. Save that two-file proof patch and its SHA-256. It must contain no other
   production change.
4. Build with `nix build -L .#installer-mainnet` or the release-owner-selected
   cluster output.
5. Record `flake.lock` SHA-256, installer SHA-256, proof patch SHA-256, Electron
   and Chromium versions, and the source identities above.

Call this artifact a **package-equivalent proof variant**, not a production or
release-candidate artifact. Task-103 must reproduce the reviewed flag removal,
and later release gates must validate the actual production artifact.

The executed proof variant is fully identified in Results below. Rebuilding the
same source is unnecessary unless the packaging strategy or probe changes;
neither its successful construction nor its fail-closed startup is release
certification.

## Positive Host Procedure

This procedure is retained for reproduction of the current portable-package
candidate. Planner must revise installation, helper, and identity checks if
`system_package` is selected; it is not valid to reuse portable assumptions for
a privileged package.

Repeat for every release-owner-confirmed matrix row. Use the same artifact hash
for all rows unless a product-owned reason requires separate artifacts.

1. Create a disposable user or VM snapshot and a fresh `HOME`. Confirm no real
   `$HOME/.daedalus/<cluster>` data is present.
2. Record only distribution ID/version, kernel release, desktop/session type,
   and relevant namespace/AppArmor settings. Do not record hostname, username,
   full home path, URLs from `/etc/os-release`, or unrelated process state.
3. Install the self-extracting artifact under the disposable `HOME`.
4. Set `INSTALL_ROOT` to the resulting `$HOME/.daedalus/<cluster>` and
   `PROBE_ROOT` to the checked-in probe directory. Create a host-local mode-0700
   evidence directory outside the installed wallet profile.
5. Confirm the launch environment does not define `ELECTRON_DISABLE_SANDBOX`
   and the wrapper does not add any forbidden switch.
6. Choose a nonexistent `PROFILE_ROOT` below `RAW_EVIDENCE`. Run the exact
   installed wrapper with an external pre-script deadline, preserving raw stderr
   only on the host. `DAEDALUS_PROBE_DEBUG=1` emits fixed milestones useful when
   native startup permits the script to load:

```bash
set +e
timeout --signal=TERM --kill-after=5s 30s \
  env -u ELECTRON_DISABLE_SANDBOX \
    DAEDALUS_PROBE_DEBUG=1 \
    DAEDALUS_PROBE_PROFILE_ROOT="$PROFILE_ROOT" \
    "$INSTALL_ROOT/libexec/electron" "$PROBE_ROOT/main.cjs" \
    >"$RAW_EVIDENCE/probe.json" 2>"$RAW_EVIDENCE/stderr.raw"
PROBE_EXIT=$?
set -e
```

7. Sanitize stderr on that same host using the same `PROFILE_ROOT` value. The
   probe refuses to reuse an existing profile path and removes the newly created
   directory during normal cleanup.

```bash
DAEDALUS_PROBE_INSTALL_ROOT="$INSTALL_ROOT" \
DAEDALUS_PROBE_ROOT="$PROBE_ROOT" \
DAEDALUS_PROBE_PROFILE_ROOT="$PROFILE_ROOT" \
DAEDALUS_PROBE_HOME="$HOME" \
node "$PROBE_ROOT/main.cjs" sanitize-stderr \
  --input "$RAW_EVIDENCE/stderr.raw" --exit-code "$PROBE_EXIT" \
  >"$RAW_EVIDENCE/stderr-summary.json"
```

8. Validate both JSON documents and inspect them for raw roots, usernames, URLs,
   hostnames, environment values, and unrelated process data before transfer.
9. Return only normalized JSON, stderr summary, artifact/source/proof-patch
   hashes, normalized host-policy fields, installed helper ownership/mode, and
   the expected/actual result. Keep raw evidence host-local and restricted.
10. At least one passing run must use the desktop-installed home-directory path,
    not a Nix store path or unpackaged Electron.

Expected positive result: exit `0`, result `pass`, all exact-renderer assertions
true, and no sandbox-disabling argument or environment state.

An external `timeout` exit (`124` or forced-kill equivalent), native signal
exit, missing probe JSON, or sanitized `<NON_PROBE_STDERR_REDACTED>` result is a
fail-closed diagnostic outcome, not positive proof.

## Restricted-Userns Procedure

This procedure is provisional until the packaging decision and matrix are
approved. A system-managed helper strategy may require a different negative
case while preserving fail-closed/no-retry behavior.

Run only in a disposable snapshotted VM. Release engineering must choose and
record the supported distribution's documented policy mechanism; do not apply
generic sysctl advice to a primary workstation.

1. Record the original namespace/AppArmor policy and snapshot identifier.
2. Deny unprivileged user namespaces through that distribution's supported
   policy. Independently confirm denial with `unshare -Ur true` or the
   distribution-approved equivalent.
3. Run the same default proof artifact and command without a sandbox bypass.
4. Require nonzero startup/probe failure. Inspect process/launcher evidence to
   confirm there was no retry with `--no-sandbox`, no host-policy alteration,
   and no successful renderer result.
5. Sanitize evidence on-host using the same fixed schema.
6. Restore the original policy and independently verify restoration, or revert
   the VM snapshot.

Expected negative result: a categorized namespace/AppArmor/sandbox startup
failure, no passing probe JSON, and no unsandboxed retry. A failure is the safe
unsupported-host outcome, not proof that ordinary wallet fallback is already
implemented.

## Evidence Record

The release owner should return one row per run:

| Field | Required value |
|---|---|
| Matrix authority | Owner and reviewed matrix revision |
| Source identity | Base commit and build-relevant task diff SHA-256 when such a diff was applied |
| Proof identity | Two-file proof patch SHA-256 |
| Package identity | Cluster, installer SHA-256, `flake.lock` SHA-256 |
| Runtime identity | Electron/Chromium versions and installed file hashes |
| Host identity | Distribution ID/version, kernel, session type only |
| Host policy | Relevant normalized userns/AppArmor values |
| Helper | Numeric owner/group and mode |
| Probe result | Exit code and normalized probe JSON or absence reason |
| Stderr | Sanitized summary only |
| Bypass check | No forbidden argv/environment/retry |
| Rollback | Negative VM policy restored or snapshot reverted |

### Results

#### Ubuntu 24.04 Package-Equivalent Run

Execution date: 2026-08-11. This is local feasibility evidence, not a release
matrix certification.

| Field | Result |
|---|---|
| Distribution | Ubuntu 24.04, x86_64, X11 |
| Kernel | `7.0.0-28-generic` |
| Base source | `ac80c32310787ed4d49283be81675ce7fe682f7f` |
| Two-file proof patch | `97d561998d3adbba3bbc2be2be8c0dd03df824ef9b0342de429469316f4a9fce` |
| `flake.lock` | `0c48b22f50e937ff7a2a92f88e96b2378db9d2894138473707eeb7b7818abf90` |
| Installer | `c9f1d8de93efd76e7b4c3e382165b431e32ff5f3692e4086157b390b72a07567` |
| Installed Electron | `fe9edd9f5069eebb2039e99a8ee99f098b93f839ce2d757dfbfd4656793c4ff9`, user-owned mode `0775` |
| Installed helper | `0abf5cce9b871567e55c1701c9efc64a3db2d8aac028d9ee5ad6b4c7a5e623da`, user-owned mode `0555` |
| Installed wrapper | `9b1d7624fd0ae38a70aaa0152077cc63f2038a3fda3114bf5ff6ee5f1e2bf426`, user-owned mode `0555` |
| Current probe | `89722d43ca8d62a5b1a6f33fe66010ee3c13d0cbcfc5751bee008b6c3b159ae4`; self-test, syntax, and focused formatting checks pass |
| Runtime versions | Electron package input `41.3.0`; Chromium runtime version was not emitted because default sandbox startup failed and the diagnostic control is not accepted version evidence |
| ELF interpreter | Installed Electron uses the package-embedded interpreter below `<INSTALL_ROOT>`; the proof exercised the relocated installed path, not Nix-store or `node_modules` Electron |
| Sandbox bypass | Both proof-only launch sites removed; installed proof wrapper contains neither bypass switch; production source remains unchanged for task-103 |
| Host userns policy | `kernel.unprivileged_userns_clone=1`, `user.max_user_namespaces=959672`, AppArmor unprivileged-userns restriction enabled |
| Independent userns check | `unshare -Ur true` exits `0` |
| AppArmor-profile diagnostic | `aa-exec -p unprivileged_userns -- unshare -Ur true` exits `1` while writing `uid_map`; useful profile-sensitivity evidence but not proof of Electron's failed operation |
| Packaged Electron result | Exits `132` from `SIGILL` without emitting the first optional post-`require("electron")` diagnostic milestone |
| Renderer evidence | None; no renderer or probe JSON was created |
| Final default stderr summary | 43 raw bytes, SHA-256 `de25f493e2a030af329f5f01121c9f9249da8508936bfa0a119d0a5c8f638731`, category `other`, excerpt `<NON_PROBE_STDERR_REDACTED>`; the wrapper diagnostic contains no accepted root-cause evidence |
| Kernel evidence | Invalid-opcode trap in the installed Electron executable; no matching Electron AppArmor audit line was emitted |
| Helper-removal diagnostic | Removing and restoring the unusable helper only in the disposable install still exits `132` |
| Unsandboxed control | Explicit local-only `--no-sandbox` reaches `app-ready`, creates and loads a renderer, records inherited `--type=zygote`, then exits `1` because the probe rejects `forbidden-main-switch`; sanitized stderr SHA-256 `64817202d927a9632ef440da35e913c96288436134ea4f273dfa02af7aa33818`, category `evidence-invalid` |
| Safety | No production/automatic bypass or host-policy change; explicit local-only control used disposable content/home and was rejected by the probe |

The result is fail-closed but not acceptance: the current package cannot start
the sandbox probe on the Ubuntu 24.04 development/evidence host. The control
isolates the failure to Chromium sandbox bootstrap but does not independently
attribute it to AppArmor, user namespaces, or another Chromium packaging detail.
The optional first milestone occurs only after `require("electron")`, so its
absence must not be used to claim that no JavaScript ran or that every future
in-process canary is impossible. Further root-cause work plus the
packaging/deployment model or supported Linux dApp scope must be resolved before
task-005 can be replanned and completed. Windows and macOS use different
Chromium OS sandbox mechanisms and remain later packaged-artifact validation
scopes; this Linux result does not invalidate those platforms.

## Unresolved Decisions And Evidence

**Resolved (2026-08-12):** Linux ships `.deb` and `.rpm` only; portable `.bin`,
AppImage, Flatpak, and Snap are rejected. See research `06`.

Still required before task-005 can complete:

1. Planner revision of the canonical task-005 plan for the system-package
   strategy (probe helper assertions, install procedure, matrix).
2. Authoritative `.deb`/`.rpm` distribution/version matrix.
3. Passing packaged exact-renderer results on installed `.deb` and `.rpm`
   artifacts for every supported row, plus snapshotted restricted-sandbox
   failure/no-retry runs and rollback evidence.
4. Implementation review after that evidence. Tracker completion and final task
   commit remain blocked until then.

The local-only `--no-sandbox` control must not be repeated as ordinary
verification, shipped, or cited as containment. Its sole accepted finding for
the portable spike is that that installed executable could reach a renderer when
sandbox bootstrap was deliberately bypassed; the probe then failed as designed.

## Provisional Task-103 Contract

This contract preserves fail-closed boundaries and assumes the accepted
`.deb`/`.rpm` install model. Before any remote dApp URL is created or loaded on
Linux, task-103 must at minimum:

1. Remove both bypass switches from development and packaged launch paths.
2. Reject sandbox-disabling argv or environment state.
3. Run a hidden local-only canary once per app process with the probe's critical
   preferences, no preload/IPC, a random nonpersistent session, and bundled
   local/data content.
4. Identify the canary renderer through `webContents.getOSProcessId()` and apply
   same-PID `/proc` assertions. Record the observed Chromium process type;
   accept the reviewed Linux zygote-inherited argv behavior rather than treating
   argv alone as renderer authority.
5. Destroy the canary and clear/release its session on success or failure.
6. Cache success in memory only. Any timeout, crash, Electron PID mismatch,
   unexpected Chromium process type, missing evidence, or failed assertion keeps
   dApp availability false.
7. Never retry unsandboxed, change host policy, or offer an in-app bypass. An
   explicit legacy unsandboxed wallet launch remains dApp-disabled.

Approved package evidence and runtime viability are separate gates. Missing or
stale package evidence prevents release enablement; a runtime canary failure
prevents launch on that host even when the package family was certified.
Native failure before application code is covered by release/package evidence
and fail-closed launch behavior; an in-process canary is not a substitute for
that gate.

## Revalidation Triggers

Reopen affected package evidence after material changes to Electron/Chromium,
nixpkgs, `nix-bundle-exe`, ELF interpreter repair, archive extraction, helper
handling, launcher flags, probe/canary assertions, supported distributions, or
the release artifact. Task-107, task-802, task-807, and task-903-a own the later
real-guest, packaged adversarial, release-candidate, and post-pilot checks.
