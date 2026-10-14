Implementation: Iteration 1
Timestamp: 2026-08-14T19:36:10Z

Changes made:
- Recorded user-approved matrix revision `task-005-a-matrix-2026-08-14`: Ubuntu 22.04.x/24.04.x/26.04.x, Debian 12.x/13.x, and Fedora 43 on x86_64; no current Ubuntu interim row; omitted/EOL rows are wallet-only and dApp-disabled.
- Froze either-route semantics: independently proven root-owned mode-4755 SUID or mode-0755 userns may pass; Ubuntu requires AppArmor, Fedora 43 requires SELinux, and Debian has no package policy asset by default.
- Upgraded the local probe to schema version 2 with pinned matrix and `/etc/os-release` checks, exact `/opt/daedalus/<cluster>` enforcement, route-specific helper and exact-renderer predicates, same-renderer policy labels, normalized process evidence, policy evidence, and expanded dependency-free fixtures.
- Synchronized research 05/06, the PRD, tracker contract wording, and canonical plan while preserving historical portable evidence and downstream package/certification/runtime ownership.

Files touched:
- `scripts/linux-chromium-sandbox-probe/main.cjs`
- `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md`
- `.agent/plans/dapp-browser-cip30/research/06-linux-system-package-decision.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005-a.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005-a-plan-review.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005-a-impl-review.md`

Verification run:
- `node scripts/linux-chromium-sandbox-probe/main.cjs --self-test` passed.
- `node --check scripts/linux-chromium-sandbox-probe/main.cjs` passed.
- Focused Prettier passed for every changed JavaScript, Markdown, and JSON file.
- Task JSON parse, unique IDs, dependency resolution, and acyclicity passed for 86 tasks.
- `git diff --check` passed.
- The repository-wide `yarn prettier` wrapper was also attempted but ignores focused path arguments and reported unrelated existing formatting drift; direct task-only Prettier passed.

Deviations from approved plan:
- None in scope. The returned matrix selected either independently proven SUID or userns route per supported row; combined evidence remains unattributed unless isolated.
- No installed artifact, host-policy mutation, package implementation, launcher flag removal, runtime canary, or guest enablement was performed.

User interaction:
- Required decision completed. The user confirmed authority, this conversation/repository record as approval, no separate reviewer, exact rows and EOL exclusions, future interim certification, route fallback semantics, policy mapping, and wallet-only omitted-host behavior. No further user interaction is required for this contract task.

Outcome: Implementation iteration 1 is complete and ready for comprehensive code review; production guest launch remains disabled and installed-artifact certification remains task-005-b.

Code Review: Iteration 1
Timestamp: 2026-08-14T19:40:33Z

Outcome: The implementation preserves contract-only scope, but the probe does not yet implement the approved evidence contract. The broad review found incomplete package identity checks, self-asserted route and policy evidence, privacy-schema violations, missing fixtures, unbounded cleanup failure, and stale downstream documentation.

Blocking findings:
1. `scripts/linux-chromium-sandbox-probe/main.cjs` does not enforce every frozen package path, ownership/mode, or expected artifact identity. Any canonical executable below `libexec` can pass; launcher/frontend checks and expected hash binding are absent.
2. `DAEDALUS_PROBE_USERNS_AVAILABLE` is caller-controlled rather than independently observed evidence, so route classification is not proven.
3. AppArmor/SELinux state, ABI, contexts, and labels are caller-controlled or permissively matched; exact parser/profile/module/file-context evidence is absent, and invented SELinux labels conflict with row-specific evidence remaining downstream.
4. Export violates the privacy contract by including numeric ownership and unnecessary labels/absolute policy paths, omitting required normalized host/relationship fields, leaving policy strings unsanitized, separating stderr from the versioned schema, and not representing normalized failures.
5. Fixture coverage omits all-prerequisites-denied, complete ownership/mode failures, policy/audit allowlisting, final-object residual leak checks, exact adversarial labels/contexts, independent userns evidence, and all matrix boundaries.
6. Renderer authority is not rechecked during collection: render exit monitoring ends after load, OS PID is not re-read, and `/proc` Pid is not matched, permitting exit/PID reuse races.
7. Final profile removal can throw after the global deadline is cleared and before listener removal/app exit, so cleanup is not internally bounded.
8. Tracker, PRD, architecture, and research retain stale Ubuntu-24.04-only AppArmor, broad RPM-family, omitted Fedora SELinux, or userns/AppArmor-only wording.

Non-blocking observations:
- Matrix keys/version patterns, stale revision rejection, common containment checks, helper symlink/mode checks, and no-bypass behavior are directionally correct.
- Production bypasses remain unchanged; no package, host policy, installed-artifact proof, or guest enablement is claimed.
- Probe self-test, syntax, focused Prettier, graph validation, and `git diff --check` pass.

Approval bar:
- Enforce exact package paths, ownership/modes, and expected artifact identities.
- Replace route/policy assertions with independently collected exact reviewable evidence without inventing unresolved labels.
- Emit one versioned allowlisted sanitized success/failure evidence schema.
- Preserve renderer authority throughout collection and bound every cleanup path.
- Add complete mechanism, policy, failure, matrix-boundary, and privacy fixtures.
- Synchronize PRD, architecture, tracker, and research to the exact matrix and Fedora SELinux contract without advancing downstream tasks or enabling guests.

Decision: requires_changes

Implementation: Iteration 2
Timestamp: 2026-08-14T19:51:43Z

Changes made:
- Added exact package path and root-owned directory/file mode checks plus a fixed root-owned identity manifest that binds matrix revision, cluster, package hashes, and task-reviewed policy identity.
- Removed caller-asserted userns and policy evidence. The probe now independently runs `unshare -Ur true`, checks AppArmor enablement/loaded enforcing profile/parser version, or checks SELinux enforcing state/module/exact file contexts, and exactly matches the renderer label to manifest-reviewed values.
- Reworked exported evidence to omit numeric ownership and unnecessary raw identifiers, tokenize package/policy paths and PIDs, include host/session and relationship fields, sanitize the final object, and emit versioned normalized failures with validated matrix context.
- Kept renderer-gone monitoring active, validated `/proc` Pid against `getOSProcessId()`, rechecked WebContents/PID after evidence collection, and kept session/profile cleanup under the process deadline with non-throwing bounded failure handling.
- Expanded fixtures for package owner/group/mode/hash/symlink failures, independent userns outcomes, all-routes-denied behavior, every matrix version boundary, exact policy labels/contexts, final evidence privacy, and normalized failures.
- Synchronized task-108/109, PRD, architecture, research 05/06, tracker, and canonical plan wording to all Ubuntu AppArmor rows, Fedora 43-only SELinux, the identity manifest, and exact matrix semantics.

Files touched:
- `scripts/linux-chromium-sandbox-probe/main.cjs`
- `.agent/system/architecture.md`
- `.agent/plans/dapp-browser-cip30/research/05-linux-chromium-sandbox-packaging.md`
- `.agent/plans/dapp-browser-cip30/research/06-linux-system-package-decision.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-prd.md`
- `.agent/plans/dapp-browser-cip30/dapp-browser-cip30-tasks.json`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005-a.md`
- `.agent/plans/dapp-browser-cip30/task-plans/task-005-a-impl-review.md`

Verification run:
- Probe self-test passed after the fix pass.
- Node syntax check passed.
- Focused Prettier passed for all changed code/docs/tracking files.
- Task JSON parse and 86-task dependency/acyclicity validation passed.
- Focused stale-contract searches leave only truthful historical Ubuntu 24.04 portable rationale and append-only review history.
- `git diff --check` passed.

Deviations from approved plan:
- Added the minimal identity-manifest handoff required to bind task-108/109 package outputs and reviewed policy labels to task-005-b evidence. It introduces no package implementation or runtime service.
- `understand-diff` was consulted, but no knowledge graph exists; the fix was verified against the complete live diff and direct files.

User interaction required: no. The approved matrix decision remains unchanged.

Outcome: All Code Review iteration-1 blockers are addressed; implementation iteration 2 is ready for focused comprehensive review and production guest launch remains disabled.

Code Review: Iteration 2
Timestamp: 2026-08-14T19:55:02Z

Outcome: Iteration 2 improves package-file verification and independently observes userns and host-policy state, but four prior blockers remain incomplete and one documentation contradiction was introduced. Installed-artifact proof remains correctly deferred to task-005-b.

Blocking findings:
1. The single evidence schema is still incomplete: success emits before cleanup and may be followed by a failure object; `sanitize-stderr` is incompatible; ordinary failures omit common fields; numeric PID-like argv values are not tokenized.
2. AppArmor accepts any parser version without manifest-reviewed ABI or exact-profile acceptance, SELinux exports only booleans rather than reviewable identities, and research 06 still invents module `daedalus_<cluster>`.
3. Synchronous collection prevents renderer-gone event processing; numeric PID rechecks do not bind immutable `/proc` process start time, so PID reuse remains possible.
4. Fixtures do not execute complete package/manifest/policy verification and omit parser/module, cleanup, PID replacement, truncation, and unified output cases; the all-routes-denied helper is not used by production behavior.
5. Current docs still contain task-108 helper “and/or AppArmor” wording, research 05 omits Fedora SELinux in its accepted summary, research 06 predeclares a conflicting module name, and architecture lacks the exact matrix revision/rows.

Non-blocking observations:
- Exact package paths, root ownership/modes, regular-file checks, directories, and manifest-bound hashes are now enforced.
- Userns, SELinux enforcement/module/file contexts, and AppArmor enablement/profile state are independently observed.
- Approved route semantics and downstream ownership remain intact; production guests remain disabled.
- Cleanup operations are timeout-wrapped, though evidence finalization is too early.
- Probe self-test, syntax, and `git diff --check` pass.

Approval bar:
- Emit exactly one final schema-v2 object after cleanup with consistent success/failure fields, integrated diagnostics, and PID-bearing argv tokenization.
- Bind AppArmor ABI/profile acceptance to reviewed manifest data, export reviewable exact policy identities, and remove invented SELinux names.
- Bind renderer identity through collection using immutable process-instance evidence and lifecycle event processing.
- Add executable complete package/manifest/policy, output/privacy, renderer replacement, cleanup, truncation, and failure fixtures.
- Synchronize every governing document to identical mandatory-policy and manifest-owned identity wording.

Decision: requires_changes

Implementation: Iteration 3
Timestamp: 2026-08-14T20:02:50Z

Changes made:
- Deferred final probe output until all session/profile cleanup completes, so each run emits exactly one schema-v2 success or failure. `sanitize-stderr` now merges host-local probe JSON and stderr into the same full envelope; missing probe evidence fails explicitly.
- Added PID-bearing argv tokenization and consistent nullable envelope fields for matrix, host, package, policy, process, assertions, failure, and diagnostics.
- Bound AppArmor parser version to the reviewed identity manifest, required non-loading parser acceptance of the exact hash-bound profile, and exported normalized exact AppArmor/SELinux identities. Removed every predeclared generic SELinux module/type name.
- Bound renderer authority to immutable `/proc/<pid>/stat` start time, rechecked it after collection, yielded to process queued lifecycle events, and rechecked WebContents/PID after the yield.
- Added executable injected fixtures for full package paths/modes/hash manifest, AppArmor parser/profile, SELinux module/contexts, renderer replacement, matrix boundaries, all-routes-denied production predicate, cleanup failures, stderr truncation, PID argv privacy, and unified final evidence.
- Corrected mandatory AppArmor wording for every Ubuntu row, Fedora 43 SELinux summaries, manifest-owned identity, and exact matrix revision/rows across tracker, PRD, architecture, research 05/06, and canonical plan.

Verification run:
- Probe self-test passed.
- Node syntax check passed.
- Focused Prettier passed for all changed code/docs/tracking files.
- Task graph parse/dependency/acyclicity validation passed for 86 tasks.
- Focused stale-contract searches now find only append-only review history and truthful historical Ubuntu 24.04 rationale.
- `git diff --check` passed.

Deviations from approved plan:
- The identity manifest now also carries the reviewed AppArmor parser version and task-109 policy identity; this is the minimal downstream handoff needed for independently reviewable exact policy evidence.
- No package, host policy, launcher, runtime canary, remote guest, or production feature state changed.

User interaction required: no.

Outcome: All Code Review iteration-2 blockers are addressed; implementation iteration 3 is ready for review and installed-artifact proof remains task-005-b.

Code Review: Iteration 3
Timestamp: 2026-08-14T20:06:11Z

Outcome: Iteration 3 resolves policy identity, PID-argv privacy, normal finalization, matrix, and most synchronization issues. Three production sequences and four current documentation statements remain incorrect.

Blocking findings:
1. Immutable renderer start time is rechecked before the lifecycle yield, but after the yield only numeric PID/state is checked. Re-read start time after the yield and fixture the production ordering.
2. Session cleanup failure returns before profile removal, leaving host-local data. Attempt all bounded cleanup steps and merge failures; fixture that profile removal still runs after session failure.
3. Empty/truncated/malformed existing `probe.json` makes sanitizer JSON parsing escape and discard the computed stderr summary. Normalize unreadable probe evidence and always merge diagnostics through the actual sanitizer path.
4. PRD packaging summary, research 06 task-108 ownership, and tracker task-108 security wording still say SUID/AppArmor without exact either-route and mandatory-policy semantics.

Non-blocking observations:
- Normal success/failure finalization, PID argv privacy, manifest-bound AppArmor/SELinux identity, exact package identity, matrix boundaries, and policy requirements are otherwise correct.
- Focused self-test, syntax, and `git diff --check` pass.
- Installed-artifact proof and all production guest gates remain downstream and disabled.

Approval bar:
- Recheck immutable renderer start time after lifecycle yield with production-order fixture.
- Attempt profile removal after session cleanup failure and fixture the sequence.
- Normalize malformed probe JSON while preserving merged stderr diagnostics through sanitizer output.
- Synchronize remaining current descriptions to either-route and mandatory-policy wording.
- Preserve all resolved security, privacy, scope, and downstream boundaries.

Decision: requires_changes

Implementation: Iteration 4
Timestamp: 2026-08-14T20:08:15Z

Changes made:
- Moved immutable `/proc` start-time and PID re-read after the event-loop yield; the production helper now checks queued renderer-gone state, WebContents identity, numeric PID, and immutable process instance in the correct order.
- Changed cleanup to attempt session clearing and profile removal independently under bounds, aggregate both failure codes, and fixture profile removal after session failure plus combined failure.
- Normalized empty, truncated, malformed, or stale-schema `probe.json` to `missing-probe-evidence` and merged the already computed stderr exit/hash/category/excerpt/truncation data into the same final envelope.
- Added production-order renderer replacement, malformed probe merge, and cleanup sequencing fixtures.
- Updated remaining PRD, research 06, and tracker descriptions to exact either-route SUID/userns, mandatory Ubuntu AppArmor/Fedora 43 SELinux, and identity-manifest wording.

Verification run:
- Probe self-test passed.
- Node syntax check passed.
- Focused Prettier passed.
- 86-task JSON dependency/acyclicity validation passed.
- `git diff --check` passed.

Deviations from approved plan: none.

User interaction required: no.

Outcome: All Code Review iteration-3 blockers are addressed; implementation iteration 4 is ready for review and all installed-artifact/production guest gates remain downstream.

Code Review: Iteration 4
Timestamp: 2026-08-14T20:10:09Z

Outcome: Iteration 4 resolves every iteration-3 blocker without regression or scope expansion. Immutable renderer identity is rechecked after the lifecycle yield; cleanup attempts both bounded stages and aggregates failures; malformed, empty, truncated, and stale-schema probe JSON retains computed diagnostics; and remaining governing descriptions use exact either-route and mandatory-policy semantics.

Blocking findings:
- None.

Non-blocking observations:
- Package paths, ownership, modes, hashes, and identity-manifest binding remain intact.
- AppArmor and SELinux identities remain manifest-reviewed and bound to the exact renderer and package files.
- PID/root privacy, normalized evidence, residual-leak rejection, matrix boundaries, and mechanism/policy fixtures remain intact.
- Ownership remains task-005-a contract, tasks 108/109 implementation, task-005-b installed certification, and task-103 runtime enforcement. Production guest launch remains disabled.
- Probe self-test, Node syntax, 86-task graph validation, and `git diff --check` pass.

Approval bar:
- Met. All production sequencing and documentation requirements are resolved, and package identity, policy identity, privacy, matrix, fixture, and scope boundaries remain preserved.

Decision: approved

