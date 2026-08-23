# Linux Chromium Sandbox Packaging Evidence

Status: **portable strategy rejected.** This note retains negative evidence from
the home-directory self-extracting `.bin` spike. The accepted Linux product
strategy is **system `.deb` and `.rpm` packages**; see
[06-linux-system-package-decision.md](./06-linux-system-package-decision.md).
Historical task-005 owns this cancelled portable spike. Task-005-a owns the
system-package, matrix, probe, and fail-closed contracts. Task-005-b completed
installed `.deb`/`.rpm` exact-renderer certification on 2026-08-23.
Production dApp launch remains disabled.

## Current Task State

- Task-005 status: `cancelled`; its negative portable evidence is retained and
  the product strategy moved to system packages.
- Task-005-a is `completed`; task-108 successor revision
  `task-108-matrix-2026-08-18` supersedes its contradicted Ubuntu predicates.
  Ubuntu 22.04.x is wallet-only pending separate proof; Ubuntu 24.04.x/26.04.x
  use row-specific semantic AppArmor checks rather than parser patch equality.
- Task-005-b status: `completed`; all five supported rows passed the installed
  exact-renderer probe, restricted policy/route cases failed closed, and rollback passed.
- Product decision (2026-08-12): ship Linux as `.deb` and `.rpm` only; reject
  portable `.bin`, AppImage, Flatpak, Snap, and other Linux channels. Durable
  record: research `06`.
- Portable spike status: package-equivalent Ubuntu 24.04 home install cannot
  start Chromium sandbox bootstrap (`SIGILL`). A local-only unsandboxed control
  isolated failure to sandbox startup. That evidence **supports rejecting
  portable packaging**; it is not proof that system packages work until retested.
- Production status: task-108's `.deb` launch surfaces are flag-free. Legacy
  portable and development Linux launch sites still contain downstream-owned
  bypasses; remote dApp launch remains disabled on every platform.
- Cross-platform boundary: this note covers Linux only. Windows and macOS remain
  tasks 802 and 807.

## Decision (superseded candidate → accepted replacement)

**Rejected candidate:** unprivileged-user-namespace-only path for the
home-directory self-extracting installer. That installer cannot establish a
root-owned mode-`4755` helper; Ubuntu 24.04 AppArmor userns policy is
path-sensitive; the portable proof variant failed sandbox bootstrap.

**Accepted strategy:** privileged system packages (`.deb` and `.rpm`) installing
to `/opt/daedalus/<cluster>` with independently proven SUID or userns
containment, mandatory AppArmor on supported Ubuntu rows, mandatory SELinux on
Fedora 43, no process-wide sandbox disablement, and a fail-closed dApp canary.
Full decision text, rejections, ownership, and migration notes:
[06-linux-system-package-decision.md](./06-linux-system-package-decision.md).

Automatically adding `--no-sandbox`, retrying unsandboxed, changing host policy
from the app, or weakening renderer assertions remains forbidden.

Task-005-a freezes the package and evidence contracts. Task-108/109 own
`.deb`/`.rpm` implementation; task-005-b owns installed-artifact certification;
those package tasks must produce launchers without sandbox-disabling switches.
Task-103 then owns remaining development/legacy bypass removal, runtime
argv/environment rejection, and the pre-remote-content canary. Task-110 owns `.bin` retirement and
auto-update migration. Task-107, task-802, task-807, and task-903-a own later
real-guest and release-candidate proof.

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

A positive result always requires all of the following from the same renderer PID:

- the PID comes from Electron's documented `webContents.getOSProcessId()` API;
- command line records Chromium's process type and no known sandbox bypass. On
  this Electron 41 Linux run, the associated renderer OS PID retained
  `--type=zygote`; therefore exact Electron PID association is authoritative and
  the probe accepts recorded `--type=renderer` or `--type=zygote` rather than
  falsely requiring argv alone to identify renderer ownership;
- `NoNewPrivs: 1`;
- `Seccomp: 2` and a positive `Seccomp_filters` count when the kernel exposes it;
- zero `CapEff`;
- PID and mount namespaces differ from the Electron main process;
- installed Electron, wrapper, helper, and probe hashes and modes are recorded.

The selected evidence route adds mechanism-specific requirements:

- `userns-only`: independently available userns; distinct renderer user
  namespace and UID/GID maps; exact regular non-symlink helper `0:0` mode `0755`;
- `suid-only`: independently unavailable userns; exact regular non-symlink
  helper `0:0` mode `4755`; no distinct user namespace/map assertion;
- `combined-unattributed`: userns available and helper `0:0` mode `4755`, but no
  claim about which route Chromium selected without a separate isolation run.

Ubuntu rows also require the exact renderer process AppArmor label and package
profile hash. Fedora 43 requires the exact renderer SELinux label, package policy
hash, and exact Electron/helper file contexts. Debian rows require no package
policy asset in the current matrix. A policy loaded for another process or file
metadata without exact renderer correlation cannot pass.

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
portable candidate. Schema version 2 instead requires an approved matrix row,
matrix revision, cluster, mechanism class, and independently observed userns
state. It independently matches `ID` and `VERSION_ID` from `/etc/os-release`
against the selected row, rejects omitted rows, and validates the corresponding
helper contract without exporting unrelated OS-release fields or URLs.

Run the dependency-free redaction golden test before host execution:

```bash
node scripts/linux-chromium-sandbox-probe/main.cjs --self-test
node --check scripts/linux-chromium-sandbox-probe/main.cjs
```

## Evidence Privacy

Raw argv, command lines, environment, paths, numeric PIDs, namespace inode IDs,
UID/GID maps, process labels, policy/audit output, and stderr remain in a mode-`0700`
directory on the disposable host and must not be committed or transferred. The
exported records use the following deterministic substitutions, longest root
first:

| Host-local root | Export token |
|---|---|
| Installed package root | `<INSTALL_ROOT>` |
| Probe directory | `<PROBE_ROOT>` |
| Temporary Electron profile | `<PROFILE_ROOT>` |
| Disposable home | `<HOME>` |

Export uses `<MAIN_PID>` and `<RENDERER_PID>`, namespace `sameAsMain` booleans,
and map `identity`/`remapped` classifications rather than raw identifiers. It
contains only the approved matrix row/revision, package family, mechanism class,
allowlisted distro/version/kernel/session fields, exact-renderer normalized
policy label, exact-file normalized contexts, package/profile hashes, and fixed
assertion results. Successes and failures use schema version 2; failures include
only fixed category/code plus the selected approved matrix context when it was
validated. Argv remains an ordered JSON string array. Stderr export contains only exit
code, byte count, SHA-256 of raw bytes, one fixed category, a sanitized UTF-8
excerpt of at most 8192 bytes, and a truncation marker when applicable. The
sanitizer applies component-boundary root replacement longest-first; removes
all URI schemes; removes usernames and hostnames case-insensitively; removes
sensitive environment-derived values and assignment values; replaces remaining
absolute paths across punctuation delimiters; and rejects export if prohibited
content remains. Policy labels and audit excerpts receive the same sanitizer and
residual-leak rejection. It retains only fixed probe/fatal/sandbox/AppArmor/userns/SELinux lines
from stderr and substitutes `<NON_PROBE_STDERR_REDACTED>` when raw stderr has no
relevant line. Raw-byte count and hash still preserve correlation. Golden tests
cover braces/brackets, `file://`, mixed-case identity data, values outside an
assignment, root tokens, residual rejection, forbidden switches, and timeout.
Do not include a token reverse map in returned evidence.

## Required Inputs

Packaging decision and successor matrix revision `task-108-matrix-2026-08-18` are
complete; see research `06`. Before task-005-b certification, release/product
engineering must provide:

1. One disposable default-policy host for every enabled matrix row.
2. Snapshotted disposable hosts where each accepted sandbox route or required
   AppArmor/SELinux policy class can
   safely be denied, including rollback access.
3. Exact `.deb` and `.rpm` proof artifacts from task-108/109 (or package-
   equivalent proof builds). The portable `.bin` proof artifact below is
   diagnostic only and is not a release candidate.
4. A host-local Node.js runtime for the sanitizer (evidence tool only).

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

## Installed System-Package Certification Handoff

Task-005-b repeats this procedure for the supported Ubuntu 24.04.x/26.04.x,
Debian 12.x/13.x, and Fedora 43 rows. Ubuntu 22.04.x receives a separate
wallet-only package lifecycle check. It uses exact installed
task-108/109 artifacts and never substitutes the historical portable package,
Nix-store Electron, or `node_modules` Electron.

1. Install the package in a disposable snapshotted VM with a fresh `HOME`.
2. Record only allowlisted distro/version, kernel release, session type, and
   matrix-required userns/AppArmor/SELinux fields in raw host-local evidence.
3. Set `INSTALL_ROOT=/opt/daedalus/<cluster>`, create a mode-0700 raw evidence
   directory outside wallet state, and verify exact package paths, ownership,
   modes, identity-manifest-bound hashes, policy assets, and flag-free launchers.
4. Select `userns-only`, `suid-only`, or `combined-unattributed` from observed
   prerequisites. A supported row may pass through either independently proven
   route. Combined evidence cannot claim route attribution without isolation.
5. Choose a nonexistent `PROFILE_ROOT` below `RAW_EVIDENCE`. Run the exact
   installed Electron wrapper with the frozen matrix inputs and an external
   deadline; preserve raw stderr only on the host:

```bash
set +e
timeout --signal=TERM --kill-after=5s 30s \
  env -u ELECTRON_DISABLE_SANDBOX \
    DAEDALUS_PROBE_DEBUG=1 \
    DAEDALUS_PROBE_PROFILE_ROOT="$PROFILE_ROOT" \
    DAEDALUS_PROBE_MATRIX_REVISION="task-108-matrix-2026-08-18" \
    DAEDALUS_PROBE_MATRIX_ROW="<ROW>" \
    DAEDALUS_PROBE_SANDBOX_CLASS="<CLASS>" \
    DAEDALUS_PROBE_CLUSTER="<cluster>" \
    "$INSTALL_ROOT/libexec/electron" "$PROBE_ROOT/main.cjs" \
    >"$RAW_EVIDENCE/probe.json" 2>"$RAW_EVIDENCE/stderr.raw"
PROBE_EXIT=$?
set -e
```

6. Sanitize stderr on that same host using the same `PROFILE_ROOT` value. The
   probe refuses to reuse an existing profile path and removes the newly created
   directory during normal cleanup.

```bash
DAEDALUS_PROBE_INSTALL_ROOT="$INSTALL_ROOT" \
DAEDALUS_PROBE_ROOT="$PROBE_ROOT" \
DAEDALUS_PROBE_PROFILE_ROOT="$PROFILE_ROOT" \
DAEDALUS_PROBE_HOME="$HOME" \
node "$PROBE_ROOT/main.cjs" sanitize-stderr \
  --input "$RAW_EVIDENCE/stderr.raw" --exit-code "$PROBE_EXIT" \
  --probe-json "$RAW_EVIDENCE/probe.json" \
  >"$RAW_EVIDENCE/final-evidence.json"
```

7. The probe independently runs `unshare -Ur true`; reads exact renderer labels;
   checks AppArmor enablement, loaded enforcing profile, manifest-reviewed parser
   version, and non-loading parse acceptance of the exact hashed profile; or
   checks SELinux enforcing state, installed reviewed module, and exact-file
   contexts against the root-owned identity manifest. Debian supplies no policy
   asset. Validate all exported JSON for residual paths, identities, raw
   PIDs/namespaces/maps, URLs, environment values, and unrelated process data
   before transfer.
8. Treat `probe.json` as host-local intermediate data. Return only the merged
   schema-v2 `final-evidence.json`, artifact/source/lock/package hashes,
   installed runtime/helper/policy identities, exact-file contexts, bypass
   checks, and expected/actual result. Keep raw evidence host-local.

Expected positive result: exit `0`, result `pass`, all exact-renderer assertions
true, and no sandbox-disabling argument or environment state.

An external `timeout` exit (`124` or forced-kill equivalent), native signal
exit, missing probe JSON, or sanitized `<NON_PROBE_STDERR_REDACTED>` result is a
fail-closed diagnostic outcome, not positive proof.

## Denied-Prerequisite Procedures

Run only in a disposable snapshotted VM. Release engineering must choose and
record the supported distribution's documented policy mechanism; do not apply
generic sysctl advice to a primary workstation.

1. Record original helper/userns/AppArmor/SELinux policy and snapshot identity.
2. For every accepted route or required policy class, deny that prerequisite
   using the distribution-supported mechanism and independently confirm denial.
3. When the other approved route remains available, require that route to pass
   exact-renderer checks without fallback flags. When all approved routes or a
   mandatory policy are denied, require dApp/package refusal as specified.
4. Confirm there is no `--no-sandbox` retry, host-policy weakening, unrelated
   policy mutation, or successful renderer result through an unapproved route.
5. Sanitize evidence on-host, restore policy and helper state, and independently
   verify restoration or revert the VM snapshot.

Expected all-routes-denied result: categorized refusal, no passing probe JSON,
no unsandboxed retry, and successful rollback. An omitted matrix row may install
wallet-only but remains dApp-disabled; a listed row whose package invariants
cannot be established fails package configuration.

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
| Host policy | Relevant normalized userns/AppArmor/SELinux values and exact-renderer/file matches |
| Helper | Root/non-root owner/group classification, mode, and manifest-bound hash |
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
the contract and certification tasks can be completed. Windows and macOS use different
Chromium OS sandbox mechanisms and remain later packaged-artifact validation
scopes; this Linux result does not invalidate those platforms.

Task-108 later root-caused this historical portable result without rewriting it:
the pinned Electron startup path intentionally traps with `ud2` when
`CHROME_DEVEL_SANDBOX` is absent. The root-managed `.deb` launcher now exports
that variable as the exact package-owned mode-`4755` helper path. A bounded
same-binary diagnostic with that value reached Electron/JavaScript startup
without `SIGILL`; this is package startup evidence, not task-005-b renderer
certification and not permission to add a sandbox bypass to the portable path.

## Unresolved Decisions And Evidence

**Resolved (2026-08-12):** Linux ships `.deb` and `.rpm` only; portable `.bin`,
AppImage, Flatpak, and Snap are rejected. See research `06`.

Task-005-a froze the contract, tasks 108 and 109 produced the packages, and
task-005-b completed the installed-artifact matrix on 2026-08-23. Task-103 is
therefore dependency-unblocked for remaining bypass removal and the runtime
canary. This certification does not enable a guest or satisfy later release gates.

The local-only `--no-sandbox` control must not be repeated as ordinary
verification, shipped, or cited as containment. Its sole accepted finding for
the portable spike is that that installed executable could reach a renderer when
sandbox bootstrap was deliberately bypassed; the probe then failed as designed.

## Provisional Task-103 Contract

This contract preserves fail-closed boundaries and assumes the accepted
`.deb`/`.rpm` install model. Before any remote dApp URL is created or loaded on
Linux, task-103 must at minimum:

1. Remove both bypass switches from remaining development and legacy portable
   launch paths. Tasks 108 and 109 own flag-free `.deb` and `.rpm` launchers
   before task-005-b certification.
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

## Installed System-Package Certification Results

The normalized evidence index is
`scripts/linux-chromium-sandbox-probe/evidence/task-005-b/index.json`. Raw
paths, process identifiers, environment, audit output, and stderr remained in
disposable virtual machines and were not committed.

| Row | Artifact | Result | Required policy |
|---|---|---|---|
| Ubuntu 24.04 | `.deb` `7115d83f…75fa7` | pass | exact renderer AppArmor label and profile semantics |
| Ubuntu 26.04 | `.deb` `7115d83f…75fa7` | pass | exact renderer AppArmor label and profile semantics |
| Debian 12 | `.deb` `7115d83f…75fa7` | pass | none |
| Debian 13 | `.deb` `7115d83f…75fa7` | pass | none |
| Fedora 43 | `.rpm` `09abf160…5c975` | pass | enforcing SELinux, module and exact file/process contexts |
| Ubuntu 22.04 | `.deb` `7115d83f…75fa7` | expected wallet-only refusal | no Daedalus profile |

Every passing renderer was identified by
`webContents.getOSProcessId()` and reported `NoNewPrivs=1`, seccomp mode 2 with
an active filter, zero effective capabilities, and separate PID and user
namespaces. All installed launch argv were free of sandbox-disabling switches.

Restricted runs unloaded the Ubuntu AppArmor profile, disabled the Fedora
SELinux module, removed the helper's privileged mode, and denied user
namespaces. Each run failed without an unsandboxed retry. Every denial record
binds the exact passing baseline, package/probe identities, independently
observed scoped mutation, no-retry result, and named rollback record.
Reapplying the exact profile/module/helper/userns state produced a passing
probe. Native AppArmor/userns failures retain `missing-probe-evidence` while
preserving that bounded context and redacting Chromium PID/time prefixes.

Certification exposed and fixed a `.deb` packaging defect: writing the wrapper
through the bundle's inherited symlink left `libexec/electron` non-regular.
The builder now removes the symlink and asserts the resulting wrapper is a
regular file. The probe also preserves fixed assertion/system failure codes,
uses the exact renderer's kernel-owned AppArmor label rather than the
root-readable global profile list, parses profiles without reading the
privileged cache, synchronously flushes pre-probe failures, binds restricted
context through `--context-json`, and exits immediately on wallet-only rejection.
