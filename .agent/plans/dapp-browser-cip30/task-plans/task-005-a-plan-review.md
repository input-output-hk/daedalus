Planner: Iteration 1
Timestamp: 2026-08-14T18:53:51Z

Plan: Created the canonical `task-005-a` plan at `.agent/plans/dapp-browser-cip30/task-plans/task-005-a.md` with planning status `in_review` and build status `in_progress`.

Interaction mode: Classified as `interactive_decision`. Implementation is blocked until release/product engineering supplies the actual release-owner-authoritative x86_64 distro/version matrix and its reviewed revision. An owner and future date are handoff metadata only, not completion evidence.

Contract scope: Freezes `.deb`/`.rpm`-only shipping under `/opt/daedalus/<cluster>`, concrete task-108/109 post-install and flag-free launcher responsibilities, exact-renderer SUID/userns/AppArmor/SELinux probe evidence, deterministic privacy controls, unsupported-host fail-closed behavior, and downstream ownership. Package implementation, artifact certification, current flag removal, runtime canary work, migration, remote content, and guest enablement remain explicitly out of scope.

Live findings: Verified the current probe still rejects a usable SUID helper and the development and legacy portable launch surfaces still pass sandbox-disabling flags. The plan limits implementation to adapting the existing probe contract and synchronizing research 05/06, the PRD, tracker, and canonical plan after the authoritative matrix is supplied.

Verification: The plan requires probe self-tests and syntax checks, focused Prettier, task-graph validation, stale-contract searches, privacy fixture review, `git diff --check`, and a complete scope review. The new plan passes focused Prettier and `git diff --check`; neither review log was modified.

Self-review: Confirmed mandatory fields, truthful manual input and evidence requirements, no owner/date substitution, no package or certification claims, preserved exact-renderer and privacy boundaries, disabled production guests, current downstream ownership, and no unnecessary package/runtime machinery.

Outcome: Plan drafted and ready for critique

Critiquer: Iteration 1
Timestamp: 2026-08-14T18:57:38Z

Outcome: The plan preserves the correct package-task boundaries and release-owner checkpoint, but it is not yet executable as an exact system-package contract. Lifecycle state, package layout, unsupported-host behavior, mechanism-specific evidence predicates, and exported privacy schema require correction before approval.

Blocking findings:
1. `task-005-a.md:20-23,202-205` classifies the task as `interactive_decision` and says implementation cannot begin before the authoritative matrix arrives, yet records build status `in_progress`. Set build status to `not_started`, state that no probe or governing-document implementation occurs before the matrix, and preserve history through later appends.
2. `task-005-a.md:41,74,81-90,141-142` still defers exact executable paths and the non-SUID helper mode. Freeze exact launcher, wrapper, Electron, adjacent helper, AppArmor asset/identity, and SELinux context/policy paths plus root ownership and SUID/non-SUID modes. The matrix selects predefined mechanisms rather than designing layout or modes.
3. `task-005-a.md:83,90,104-108` does not distinguish a supported-row setup failure from a distro/version absent from the matrix. Freeze whether each case refuses package configuration or permits wallet-only installation with dApps unavailable, and leave task-103 only runtime enforcement of that frozen result.
4. `task-005-a.md:95` incorrectly applies distinct user/PID/mount namespaces and UID/GID maps universally to SUID and userns routes. Define reviewed mechanism-specific predicates: common exact-WebContents PID, no-bypass, `NoNewPrivs`, seccomp, and zero-capability checks; userns-specific namespace/map evidence; and SUID-specific bootstrap/containment evidence with userns independently unavailable. Keep combined evidence unattributed without an isolation run.
5. `task-005-a.md:98-100` records profile and context metadata without binding AppArmor or SELinux proof to the exact renderer. Require same-PID `/proc/<pid>/attr/current` or equivalent authoritative process-label evidence plus exact executable/helper file contexts and profile/policy hashes.
6. `task-005-a.md:95-101,157` underspecifies privacy for PIDs, namespace inode identifiers, UID/GID maps, kernel/host fields, and policy labels. Freeze an allowlisted normalized schema that tokenizes PIDs and roots, exports relationships rather than unnecessary identifiers, retains only matrix-required fields, and applies residual-leak rejection to AppArmor/SELinux and audit-derived excerpts.
7. `task-005-a.md:151-159` lacks fixture coverage for every mechanism and failure class. Require fixtures for SUID-only, userns-only, combined-unattributed, required AppArmor, required SELinux, unsupported row, helper ownership/mode/symlink failure, process-label mismatch, and all-prerequisites-denied behavior. Task-005-b must execute every authoritative row and a denied case for each distinct accepted mechanism/policy class.

Non-blocking observations:
- `interactive_decision` is otherwise truthful because the missing exact support matrix and per-row policy selections are release/product decisions.
- The actual reviewed matrix revision, authority, reviewer record, exact rows, and explicit omitted-row treatment correctly prevent owner/date metadata from satisfying acceptance.
- Ownership is correctly separated across task-005-a, tasks 108/109, task-005-b, and task-103.
- Historical task-005 remains negative portable evidence, installed-artifact execution remains out of scope, and production guests remain disabled.
- Synchronizing research 05/06, PRD, tracker, and canonical plan while deferring workflow migration is appropriately scoped.
- Reusing the dependency-free probe without new package/runtime frameworks is the preferred minimal approach.

Approval bar:
- Correct lifecycle state and enforce the interactive-decision stop before implementation.
- Freeze exact package paths, helper ownership/modes, policy assets, and separate supported-failure versus unsupported-host behavior.
- Replace the universal namespace predicate with reviewed mechanism-specific SUID/userns/common pass criteria.
- Bind AppArmor and SELinux proof to the exact authoritative renderer PID and package files.
- Freeze a privacy-safe normalized export schema covering current `/proc` and host-policy fields.
- Expand synthetic and downstream certification coverage across every accepted mechanism/policy and fail-closed branch.
- Preserve the matrix requirement, task boundaries, documentation synchronization, no-bypass rule, and disabled production guest posture without broader machinery.

Decision: requires_changes

Planner: Iteration 2
Timestamp: 2026-08-14T19:02:17Z

Lifecycle: Set build status to `not_started` and prohibited probe or governing-document implementation before the authoritative reviewed matrix is supplied.

Frozen contract: Fixed exact launcher, wrapper, Electron, helper, AppArmor, and SELinux asset paths, ownership, and SUID/non-SUID modes independently of matrix selection.

Failure behavior: Supported-row setup failures refuse package configuration. Omitted or unsupported hosts permit only wallet-only installation with dApps unavailable, consistent with the PRD.

Evidence: Replaced universal namespace assertions with common, userns-only, SUID-only, and combined-unattributed predicates. AppArmor and SELinux proof now binds the exact renderer PID to package profile/policy hashes and exact executable/helper contexts.

Privacy: Froze a normalized allowlist covering tokenized PIDs and roots, namespace/map relationships, kernel/host fields, process labels, file contexts, and bounded sanitized policy/audit excerpts with residual-leak rejection.

Coverage: Required dependency-free fixtures for every accepted mechanism/policy class and named failure branch. Task-005-b must execute every authoritative row and a denied case for each accepted mechanism or required policy class.

Preserved constraints: Retained the actual reviewed-matrix gate, contract-only scope, downstream ownership, no bypass or host-policy weakening, and disabled production guests. Package mechanics, certification, runtime enforcement, and migration remain with tasks 108/109/005-b/103/110.

Self-review: Verified live bundle assumptions, focused Prettier, lifecycle and path consistency, blocker coverage, and scope. Only the canonical plan was revised; neither append-only review log was modified.

Outcome: Canonical plan revised to resolve the complete Critiquer iteration-1 approval bar and ready for focused approval.

Critiquer: Iteration 2
Timestamp: 2026-08-14T19:03:51Z

Outcome: The focused iteration-2 check confirms that all seven original blockers are resolved without regression or unnecessary machinery.

Blocking findings:
- None.

Non-blocking observations:
- Lifecycle is consistently `not_started`, with implementation blocked until the authoritative reviewed matrix is supplied.
- Exact package paths match the live Linux bundle shape; current portable/development bypasses remain correctly assigned downstream.
- The matrix remains intentionally absent and is not replaced by owner/date metadata or inferred distro families.

Approval bar:
- Met. The revision freezes exact layout, ownership, modes, policy assets, supported-versus-unsupported behavior, mechanism-specific evidence predicates, exact-renderer policy proof, privacy-safe export schema, complete fixture/certification coverage, task boundaries, no-bypass behavior, and disabled production guests.

Decision: approved

