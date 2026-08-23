# Task-005-b Installed Linux Sandbox Certification

## Scope

Certify the exact installed task-108 `.deb` and task-109 `.rpm` artifacts against matrix revision `task-108-matrix-2026-08-18`. Remote guests remain disabled. Historical portable `.bin` evidence is not certification.

## Plan

1. Verify source, lock, package, runtime, helper, wrapper, policy, probe, and host-image identities before execution.
2. Install the exact packages on disposable x86_64 Ubuntu 24.04, Ubuntu 26.04, Debian 12, Debian 13, and Fedora 43 virtual machines with fresh user state.
3. Run the local-only probe through the exact installed Electron wrapper. Bind results to `webContents.getOSProcessId()` and require `NoNewPrivs=1`, seccomp mode 2 with a filter, zero effective capabilities, separate PID/user namespaces, exact package files, and required AppArmor or SELinux state.
4. Exercise AppArmor, SELinux, helper, and user-namespace denials. Require a categorized fail-closed result, no sandbox-disabling retry, no host-policy weakening, and a passing post-restoration probe.
5. Verify Ubuntu 22.04 remains wallet-only and cannot produce supported-row renderer evidence.
6. Export schema-v2 normalized evidence only. Keep raw paths, process identifiers, environment, policy/audit output, and stderr on disposable hosts.
7. Update the PRD, tracker, package research, and evidence handoffs only after every supported row and rollback passes.

## Acceptance Mapping

- Supported matrix: normalized positive records for Ubuntu 24.04/26.04, Debian 12/13, and Fedora 43.
- Reproducible privacy-safe identity: `scripts/linux-chromium-sandbox-probe/evidence/task-005-b/index.json` plus per-run schema-v2 records.
- Restricted sandbox and rollback: AppArmor, SELinux, helper-mode, and userns denial/restoration records in the same directory.
- Mechanisms and policy: each positive record binds helper/userns observations and exact AppArmor/SELinux renderer evidence.
- Release ordering: task-103 becomes dependency-unblocked, but production guest launch remains disabled behind its runtime canary and all later PRD gates.

## Implementation Findings

Certification exposed four source defects rather than suppressing probe failures:

- The `.deb` builder wrote the wrapper through an inherited symlink. It now removes that symlink first and asserts a regular wrapper before packaging.
- Assertion failures with Node-generated detail lost their fixed code. Failure normalization now preserves the safe first-line code and safe system error class.
- The unprivileged renderer probe could not read the root-only global AppArmor profile list or parser cache. It now relies on the exact renderer's kernel-owned AppArmor label for attachment/load proof and parses the hashed profile with cache reads disabled. Package installation still performs the privileged global profile-load check.
- Native pre-probe failures could leak Chromium PID/time prefixes, lose piped output on forced exit, and omit matrix/package identity. The sanitizer now redacts those prefixes, failure writes are synchronous, and `--context-json` binds a passing exact baseline, scoped denial, no-retry observation, and rollback record to native or in-probe denial evidence.

## Verification

- `node scripts/linux-chromium-sandbox-probe/main.cjs --self-test`
- `node --check scripts/linux-chromium-sandbox-probe/main.cjs`
- `nix build -L .#deb-installer-mainnet`
- Exact installed positive and restricted-policy probe runs indexed under `evidence/task-005-b/`
- JSON parse/privacy/invariant validation for every committed evidence record
- Focused repository formatting and task-graph validation
