Planner: Iteration 1
Timestamp: 2026-08-11T18:00:18Z

Plan: Created the canonical `task-005` plan with `manual_execution` interaction mode. It selects unprivileged user namespaces as the strategy to validate, defines an exact-renderer packaged probe using OS-level `/proc` evidence, and defers production flag removal and fail-closed runtime gating to `task-103`.

User handoff: Release engineering must provide the authoritative Linux distribution matrix, disposable positive and restricted-userns hosts, and the release-equivalent installer artifact. Agent-executable probe, research, build, and static-inspection work can proceed first, but completion remains blocked on returned packaged-host evidence.

Security and scope: The plan rejects `process.sandboxed` as sole proof, rejects SUID-helper assumptions for the home-directory installer, permits no automatic unsandboxed fallback, keeps remote content and production guest launch disabled, and leaves real guest validation to downstream tasks.

Self-review: Confirmed required task-plan fields, expected evidence and files, review-log paths, iteration-stable verification wording, planning status `in_review`, build status `in_progress`, and no review-log modifications.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-08-11T18:04:31Z

Outcome: The plan is security-conscious and appropriately scoped, but four contradictions leave the unsupported-host contract, evidence collection, privacy boundary, and downstream ownership insufficiently executable.

Blocking findings:
1. `task-005.md:38,126,130` does not freeze a concrete task-103 host-viability gate. Line 38 also incorrectly says dApp launch is unavailable when package proof "is present," while line 126 says "absent proof." Detecting only sandbox-disabling argv/environment cannot identify hosts where user namespaces are denied by kernel or AppArmor policy. Require a local sandbox canary or another evidence-backed mechanism that runs before remote content and fails dApp launch closed without an unsandboxed retry.
2. Downstream ownership is materially wrong. `task-005.md:44,55` assigns broader packaged revalidation to `task-800`, but task-800 is ledger/parser fuzzing. The real packaged production-guest owners are task-107 and task-802, followed by task-807 release-candidate gating and task-903-a post-pilot change control. The revalidation contract must name those owners accurately.
3. The probe design is internally inconsistent. `task-005.md:81-82,105,107` requires collecting renderer `process.sandboxed` while forbidding any preload or IPC. With `nodeIntegration: false` and context isolation, the page cannot reliably read the Electron renderer process object. Simplify by omitting this supplementary assertion, or permit a probe-only least-authority preload and narrowly correlated probe IPC; never weaken the required same-PID `/proc` evidence.
4. The evidence privacy contract conflicts with itself. `task-005.md:26,100,106,114,155-159` requests exact argv, raw command lines, paths, and stderr while also prohibiting usernames and home paths. Define deterministic tokenization such as `<INSTALL_ROOT>` and `<PROBE_ROOT>`, sanitize stderr and command lines before evidence leaves the disposable host, and retain hashes plus normalized argv so artifact correlation remains reproducible without committing personal paths.

Non-blocking observations:
- `manual_execution` is truthful because completion depends on a release-owner-supported distribution matrix, disposable hosts, host-policy manipulation, and immutable packaged-artifact evidence.
- Live files and history support the baseline: both launch paths pass `--disable-setuid-sandbox --no-sandbox`; commit `bfaaf79f7` added the development flags to match production; commit `d17ba5061` introduced the home-installed portable package and packaged bypass.
- The evaluated Electron helper is root-owned mode `0555` in the Nix store, while home extraction cannot establish a root-owned mode-`4755` helper. Selecting user namespaces rather than redesigning installation around SUID is the narrowest viable strategy.
- Exact renderer correlation through `webContents.getOSProcessId()`, matching `--type=renderer`, and same-PID no-new-privileges, seccomp, capabilities, and namespace evidence correctly avoids treating `sandbox: true`, startup success, or a utility process as proof.
- The AppArmor/default-host and unavailable-userns cases, prohibition on automatic bypass, disposable-HOME warning, local-only probe, immutable artifact identity, and deferred production edits are appropriate.
- Research, PRD, tracker, and task-plan synchronization is correctly delayed until manual evidence and implementation review approval. Workflow consultation and the decision not to update system/workflow documentation prematurely are appropriate.
- Calling a temporarily patched artifact "release-equivalent" is imprecise but manageable if it is labeled a package-equivalent proof variant and identified by HEAD, complete patch hash, lock hash, and artifact hash. Task-103 and later release gates must certify the actual production artifact.

Approval bar:
- Resolve the present/absent-proof contradiction and specify an implementable pre-remote-content host sandbox-availability check owned by task-103.
- Replace task-800 with task-802 and explicitly preserve task-107, task-807, and task-903-a ownership of real-guest, release-candidate, and post-pilot revalidation.
- Make `process.sandboxed` collection implementable without broadening authority, preferably by dropping the unnecessary supplementary assertion.
- Define deterministic evidence redaction that preserves artifact and argv correlation while excluding usernames, home paths, unrelated process data, and sensitive stderr.
- Preserve all other current acceptance criteria, manual blockers, no-bypass behavior, package/PID evidence, and deferred production scope without adding broader packaging redesign.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-11T18:09:44Z

Revisions: Corrected the package-proof contradiction by separating the release gate from runtime host viability. The plan now requires task-103 to run a hidden, local-only sandbox canary before any remote content, validate its exact renderer PID through same-PID `/proc` evidence, fail dApp launch closed on any failure, and never retry unsandboxed.

Ownership: Reassigned downstream packaged validation to task-107, task-802, task-807, and task-903-a. Task-800 is no longer assigned sandbox revalidation work.

Probe simplification: Removed `process.sandboxed` collection and retained the stronger main-collected exact-renderer evidence for no-new-privileges, seccomp, capabilities, and namespace identity. No probe preload or IPC is introduced.

Privacy: Defined deterministic `<INSTALL_ROOT>`, `<PROBE_ROOT>`, `<PROFILE_ROOT>`, and `<HOME>` substitution; ordered argv normalization; fixed stderr categorization, hashing, sanitization, and truncation; host-local retention of raw evidence; and fail-closed residual-leak validation.

Preserved constraints: Retained `manual_execution`, release-owner matrix and disposable-host blockers, package/helper and exact-renderer evidence, explicit wallet-only fallback behavior, no automatic sandbox bypass, deferred production edits, disabled production guest launch, and all unaffected acceptance criteria.

Self-review: Confirmed the revision changes only `task-005.md`, introduces no production implementation or packaging redesign, keeps lifecycle status truthful, and passes `git diff --check`.

Outcome: Canonical plan revised to resolve the complete Critiquer iteration-1 approval bar and is ready for focused approval.

Critiquer: Iteration 2
Timestamp: 2026-08-11T18:11:34Z

Outcome: The revised plan resolves the complete iteration-1 blocker list without introducing a new issue.

Blocking findings:
- None.

Non-blocking observations:
- Live verification confirms the sandbox-disabling launch flags, home-directory installer model, explicit wallet fallback, and downstream task ownership described by the plan.
- Manual execution, package/helper and exact-renderer PID evidence, deferred production scope, disabled production guest launch, and unaffected acceptance criteria remain preserved.

Approval bar:
- Met. Task-103 now owns a pre-remote-content, local-only, same-PID sandbox canary that fails closed without retry or bypass.
- Task-107, task-802, task-807, and task-903-a ownership is corrected.
- Inaccessible `process.sandboxed` collection is removed without adding preload or IPC authority.
- Evidence export now has deterministic root tokenization, ordered argv normalization, sanitized and hashed stderr, host-local raw-data retention, and fail-closed residual-leak checks.
- No automatic unsandboxed retry, production implementation, packaging redesign, or premature completion claim was introduced.

Decision: approved

