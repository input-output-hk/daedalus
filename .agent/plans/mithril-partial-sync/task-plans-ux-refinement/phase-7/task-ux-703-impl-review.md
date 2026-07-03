# task-ux-703 — implementation review log (append-only)

*(no entries yet — build not started)*

---

## Implementation — CAT-A chunk 1 (T5 cross-session recovery snapshot adoption)

- **Label:** Implementation
- **Scope:** CAT-A chunk 1 — plan section 1 (Steps 1.1–1.4): `adoptRecoverySnapshot` on the service, controller recovery-handler seeding, service repro/guard specs, controller wiring specs.
- **Timestamp:** 2026-07-03T17:26:10Z
- **Outcome:** Completed. All four steps applied exactly as planned; every quoted anchor matched the working tree byte-for-byte. No deviations.
- **Files changed:**
  - `source/main/mithril/MithrilPartialSyncService.ts` (Step 1.1 — `adoptRecoverySnapshot` inserted above `restartNormal`, direct `_status` assignment, no re-emit)
  - `source/main/mithril/MithrilController.ts` (Step 1.2 — `restartNormal`/`wipeAndFullSync` handlers seed the service with `this.getPartialSyncStatus()` before delegating)
  - `source/main/mithril/MithrilPartialSyncService.spec.ts` (Step 1.3 — `adoptRecoverySnapshot (cross-session recovery boundary)` describe, 5 tests)
  - `source/main/mithril/MithrilController.spec.ts` (Step 1.4 — 3 new mock handles, 3 mock-factory entries, `recovery-action wiring (cross-session snapshot adoption)` describe, 2 tests)
- **Tests run:** `yarn test:jest --testPathPattern "source/main/mithril/(MithrilPartialSyncService|mithrilPartialSyncNodeStartup|MithrilController)\.spec\.ts"` — 3 suites, 89 tests, all green; the 7 new tests verified by name via `-t "snapshot" --verbose`.
- **Notes:** All four touched files carry pre-existing prettier 2.1.2 drift at HEAD (verified via `git show HEAD:<file> | prettier --check`); the new hunks themselves are prettier-clean (prettier-diff regions all fall outside my hunks). No reformatting beyond the planned edits. `yarn compile` + boundary-net jest run deferred to end of chunk 2 per the plan's Verification section. No i18n changes (none planned for CAT-A). Nothing staged/committed.

---

## Code Review — CAT-A chunk 1 round 1 (T5 cross-session recovery snapshot adoption)

- **Label:** Code Review
- **Scope:** CAT-A chunk 1 round 1 — plan section 1 (Steps 1.1–1.4) of `task-ux-703-plan-cat-a.md`.
- **Timestamp:** 2026-07-03T17:28:24Z
- **Decision:** APPROVED
- **Checks performed:**
  - Step 1.1: `adoptRecoverySnapshot` inserted verbatim immediately above `restartNormal` in `MithrilPartialSyncService.ts`; direct `_status` assignment (no `_updateStatus`, no re-emit); `MithrilPartialSyncStatusSnapshot` already imported (no import added). Idle-only adoption of a non-idle snapshot with at least one allowed action — copies the backend-broadcast snapshot verbatim, no action computation (boundary 2 upheld).
  - Step 1.2: both recovery handlers in `_getPartialSyncDependencies()` replaced verbatim; seed via `this.getPartialSyncStatus()` (verified defined on the controller, returns `_partialSyncStatus`); no channel import added; the other seven handlers untouched.
  - Step 1.3: `adoptRecoverySnapshot (cross-session recovery boundary)` describe (5 tests) inserted verbatim directly after the `rejects start reuse from wipe-only failed boundaries` test; `removeMock` and the snapshot type verified in scope.
  - Step 1.4: three mock handles after `mockForceKillForShutdown`, three factory entries after the `forceKillForShutdown` line (no duplicate keys in the factory), wiring describe (2 tests, adopt-before-delegate call-order assertions) after the `reapPartialSyncOnShutdown` describe inside the outer describe — all verbatim.
  - No task/finding IDs in source comments or test titles; no user-facing copy, no i18n changes (as planned for CAT-A); no edits to `mithrilPartialSyncNodeStartup.ts` or any file outside the chunk; `.gitignore` / `.agent/skills/` untouched by this chunk.
  - Implementer-reported jest run (3 suites, 89 tests green) is plausible against the diff; `yarn compile` + boundary net correctly deferred to end of chunk 2 per the plan's Verification section.
- **Findings:** none.
- **Action:** staged the chunk's four source files plus this log.

---

## Implementation — CAT-A chunk 2 (T6/T8/T9/T10 startup/cancel/staging hardening; T14 verify-only)

- **Label:** Implementation
- **Scope:** CAT-A chunk 2 — plan section 2 (Steps 2A.1–2A.5, 2B.1–2B.4, 2C): startup staging-reclaim best-effort (T6), Quit-branch `safeExitWithCode` (T8), finalizeCancel best-effort cleanup landing on `cancelled` (T9), marker-first staging-root resolution in cleanup/wipe and the startup wipe branch (T10), plus their unit tests. T14 checklist-only verification.
- **Timestamp:** 2026-07-03T17:34:29Z
- **Outcome:** Completed. All steps applied exactly as planned; every quoted anchor matched the working tree byte-for-byte. No deviations.
- **Files changed:**
  - `source/main/mithril/mithrilPartialSyncNodeStartup.ts` (Steps 2A.1–2A.4 — logger/safeExitWithCode imports, C2 reclaim try/catch, wipe-branch staging reclaim, Quit branch exits via `safeExitWithCode(0)` and still returns true)
  - `source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts` (Step 2A.5 — logging + safeExitWithCode module mocks and handle; new T6 reclaim-failure test; new T10 wipe-branch staging-reclaim test; T8 Quit test replaced to pin `safeExitWithCode(0)`; stale-generation test now asserts no exit)
  - `source/main/mithril/MithrilPartialSyncService.ts` (Steps 2B.1–2B.3 — marker-first resolution in `_cleanupPartialSyncArtifacts` and `wipeAndFullSync` (marker retained until `finalizeWipeAndFullSync`); `finalizeCancel` cleanup failure now logs a warn and both exits land on `cancelled` with `['retry', 'restart-normal']`; no wipe restored into cancel)
  - `source/main/mithril/MithrilPartialSyncService.spec.ts` (Step 2B.4 — failure-path test replaced to pin `cancelled` landing; observability warn test retitled with the new warn text; new `staging root resolution from the durable marker` describe, 2 tests)
- **T14 (verify-only, no code):** all three anchors present — `process.kill(-pid, signal)` at `killProcessTree.ts:72`; `detached: !environment.isWindows` at `mithrilCommandRunner.ts:162` and `:258`; the `// Detach on POSIX so the child leads its own process group` rationale comments at `:155` and `:251`. Nothing changed.
- **Tests run:**
  - `yarn test:jest --testPathPattern "source/main/mithril/(MithrilPartialSyncService|mithrilPartialSyncNodeStartup|MithrilController)\.spec\.ts"` — 3 suites, 93 tests, all green.
  - Boundary net `yarn test:jest --testPathPattern "source/main/(mithril|ipc|utils)/.*\.spec\.ts"` — 21 suites, 366 tests, all green (bootstrap + coordinator + channel specs included).
  - `yarn compile` — clean (scss type generation succeeded this run; no typed-scss-modules workaround needed).
- **Notes:** All three edited-and-flagged files carry pre-existing prettier 2.1.2 drift at HEAD (re-verified via `git show HEAD:<file> | prettier`); the only prettier-2.1.2 complaint inside a new hunk is the `readMithrilPartialSyncMarkerMock` require in the new spec describe, which is byte-identical in style to the pre-existing occurrence in the `finalizeCompletedPartialSync` describe that 2.1.2 also flags — left as-is per the no-reformat-beyond-your-hunks rule. No i18n changes (none planned for CAT-A). Nothing staged/committed.

---

## Code Review — CAT-A chunk 2 round 1 (T6/T8/T9/T10 startup/cancel/staging hardening; T14 verify-only)

- **Label:** Code Review
- **Scope:** CAT-A chunk 2 round 1 — plan section 2 (Steps 2A.1–2A.5, 2B.1–2B.4, 2C) of `task-ux-703-plan-cat-a.md`.
- **Timestamp:** 2026-07-03T17:37:53Z
- **Decision:** APPROVED
- **Checks performed:**
  - Step 2A.1: `logger` + `safeExitWithCode` imports added after the marker import block; no electron `app` import, no new `NodeStartupDependencies` member (T8 invariant upheld).
  - Steps 2A.2/2A.3: node-start-verified reclaim and the startup wipe branch both wrap `fs.remove(marker.stagingRootPath)` in verbatim try/catch with the planned rationale comments and warn lines; both still clear the marker and return `false`; wipe + clear behavior otherwise untouched.
  - Step 2A.4: Quit tail replaced verbatim — warn with `{ markerState: marker.state }`, `safeExitWithCode(0)`, still `return true`; the earlier stale-generation `return true` guard untouched.
  - Step 2A.5: both module mocks and the `safeExitWithCode` handle added verbatim; T6 reclaim-failure test (EBUSY, returns false, marker cleared, no dialog, no exit), T10 wipe-branch staging-reclaim test, T8 Quit test replacement pinning `safeExitWithCode(0)`, and the stale-generation no-exit assertion — all verbatim in the planned positions.
  - Step 2B.1/2B.2: marker-first staging resolution in `_cleanupPartialSyncArtifacts` and `wipeAndFullSync` verbatim, with `?? this._getStagingRootPath()` fallback; `wipeAndFullSync` does NOT clear the marker (finalization ownership preserved; pinning test untouched); `finalizeCompletedPartialSync` untouched.
  - Step 2B.3: `finalizeCancel` full method matches the plan byte-for-byte (read back from the file): cleanup failure is caught, warned with the new text + `cancelFallbackErrorStage`, and BOTH exits land on `cancelled` with `['retry', 'restart-normal']`, `error: null`; no wipe restored into cancel (DD locked cancel-cleanup constraint upheld); `abandonCancel` untouched and still owns the unrecoverable `failed` path; actions remain backend-emitted only.
  - Step 2B.4: failure-path test replaced verbatim (`cancelled` landing); observability test retitled with the new warn text and `expect.objectContaining` shape; `staging root resolution from the durable marker` describe (2 tests) inserted directly after the `finalizeCompletedPartialSync` describe closing at the planned position.
  - 2C (T14): no code change; anchors independently re-verified — `process.kill(-pid, signal)` at `killProcessTree.ts:72`, `detached: !environment.isWindows` at `mithrilCommandRunner.ts:162`/`:258`, POSIX process-group rationale comments at `:155`/`:251`; neither file has staged or unstaged diffs.
  - No task/finding IDs in source comments or test titles (the `C2 branch:` prefix is the spec file's pre-existing branch-label convention, plan-mandated); no user-facing copy added or changed (native dialog text left for the copy-owning section as planned); no i18n changes; no edits outside the chunk's four files + this log; `.gitignore` / `.agent/skills/` untouched by this chunk.
  - Implementer-reported runs (3 suites / 93 tests green; boundary net 21 suites / 366 tests green incl. bootstrap; `yarn compile` clean) are plausible against the diff; Verify phase re-runs everything.
- **Findings:** none.
- **Action:** staged the chunk's four source files plus this log.

---

## Implementation — CAT-B chunk 1 (T11 + N7 availability visibility end-to-end)

- **Label:** Implementation
- **Scope:** CAT-B chunk 1 — plan section 1 (Steps 1–20) of `task-ux-703-plan-cat-b.md`: optional `isProbeFailed` on the availability type, service probe-catch flag, controller pass-through (test only), store observable + apply, container/Diagnostics prop wiring (N7 inner re-check removed, outer kill-switch gate intact), section always-render with three-variant computation, recommendation variant-driven tooltip with two new intl messages, EN + JA locale keys, catalog regeneration, and the chunk's four planned specs.
- **Timestamp:** 2026-07-03T17:51:45Z
- **Outcome:** Completed. Every quoted anchor matched the working tree; all plan steps applied as written. One out-of-plan spec fix (see Deviations).
- **Files changed:**
  - `source/common/types/mithril-partial-sync.types.ts` (Step 1 — optional `isProbeFailed` appended after `isSignificantlyBehind`; error-code union untouched)
  - `source/main/mithril/MithrilPartialSyncService.ts` (Steps 2–3 — return type + catch now returns `isProbeFailed: true`)
  - `source/main/mithril/MithrilController.ts` — NO edit (Step 4 verified: probe result spread at `return { isEnabled, ...behindness };`; the two early returns intentionally left without the flag). File shows staged CAT-A changes only.
  - `source/renderer/app/stores/MithrilPartialSyncStore.ts` (Steps 5–6 — `isProbeFailed` observable + `_applyAvailability` Boolean coercion)
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` (Step 7 — new prop pass-through)
  - `source/renderer/app/components/status/DaedalusDiagnostics.tsx` (Steps 8–10 — Props type, destructure, prop replacement; N7 inner `isMithrilPartialSyncEnabled` re-check removed; outer kill-switch gate verified intact)
  - `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` (Steps 11–12 — `isSignificantlyBehind`/`isProbeFailed` props, variant type import, early return replaced by variant computation, variant passed to recommendation; CTA `disabled` still driven only by `isActionBlocked`; `'Unable to start Mithril partial sync.'` fallback untouched for CAT-E)
  - `source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx` (Steps 13a–13c — `recommendationNearTip`/`recommendationUnknown` messages, exported `MithrilAvailabilityVariant`, `variant` prop, tooltip selection)
  - `source/renderer/app/i18n/locales/en-US.json` + `ja-JP.json` (Steps 14–15 — two keys each, alphabetical position after `...Recommendation`)
  - `translations/messages.json` + `source/renderer/app/i18n/locales/defaultMessages.json` (Step 16 — regenerated by `yarn i18n:manage`; diff contains exactly the two new message entries in each)
  - Specs per Steps 17–20: `MithrilPartialSyncService.spec.ts` (updated degrade test + new local-read-rejection test), `MithrilController.spec.ts` (pass-through test), `MithrilPartialSyncStore.spec.ts` (apply/clear test), `MithrilPartialSyncSection.spec.tsx` (defaultProps swap, gated-off test replaced by two visibility tests, deep-link test renamed/re-keyed)
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx` — OUT-OF-PLAN (see Deviations)
- **Tests run:**
  - `yarn test:jest --testPathPattern "MithrilPartialSyncService"` — 1 suite, 69 tests green
  - `yarn test:jest --testPathPattern "MithrilController"` — 1 suite, 11 tests green
  - `yarn test:jest --testPathPattern "MithrilPartialSyncStore"` — 1 suite, 36 tests green
  - `yarn test:jest --testPathPattern "MithrilPartialSyncSection|DaedalusDiagnostics"` — 3 suites, 29 tests green (section 15, diagnostics 12, dialog container 2)
  - Boundary-11 guard `yarn test:jest --testPathPattern "MithrilBootstrap"` — 5 suites, 73 tests green (no bootstrap file touched)
  - `yarn i18n:manage` — exit 0; the two new keys extracted cleanly; the check's untranslated-catalog listing is pre-existing and untouched by these keys
  - `yarn compile` — clean (scss type generation succeeded this run; no typed-scss-modules workaround needed)
- **Deviations:**
  - `DaedalusDiagnostics.spec.tsx` (not in the plan's Files touched list) broke on the chunk's own changes: (a) `tsc` failed because the spec's `defaultProps` and an inline `<DaedalusDiagnostics ...>` rerender lack the new required `isMithrilPartialSyncProbeFailed` prop — added `isMithrilPartialSyncProbeFailed: false` to `defaultProps` (the rerender spreads it); (b) the test `'shows the recommendation/CTA only when enabled and significantly behind'` asserted the old hide-when-near-tip behavior that Steps 11–12 deliberately remove — retitled to `'keeps the recommendation/CTA visible when enabled but not significantly behind'` and its final assertions now expect the CTA to remain enabled (kill-switch hide test untouched and green). Both are mechanical consequences of the approved always-render behavior, not new decisions.
  - Prettier 2.1.2: two of my hunks were reflowed to the repo formatter's output (`isSignificantlyBehind={...}` prop in `DaedalusDiagnostics.tsx` to the three-line form; the section `render()` destructure to the multi-line form). Remaining 2.1.2 warnings in `MithrilPartialSyncService.ts(.spec)`, `MithrilController.spec.ts`, `MithrilPartialSyncStore.spec.ts`, and `DaedalusDiagnostics.tsx` are pre-existing drift outside my hunks (verified via `git show :<file> | prettier --stdin-filepath`) — left untouched per the no-reformat-beyond-your-hunks rule.
- **JA copy note (flagged per plan):** the near-tip first sentence 「ノードはブロックチェーンの先端に近い状態です。」 has no exact copy-table precedent (derived from row 3's tip-position pattern); used exactly as the plan wrote it.
- **Notes:** Vocabulary check clean — new user-facing strings say "Mithril Sync", no percentages, no "immutable", no task/thread IDs in comments or test titles. `Diagnostics.stories.tsx` needs no edit (its props object is cast `as any`). Nothing staged/committed.

---

## Code Review — CAT-B chunk 1 round 1 (T11 + N7 availability visibility end-to-end)

- **Label:** Code Review
- **Scope:** CAT-B chunk 1 round 1 — plan section 1 (Steps 1–20) of `task-ux-703-plan-cat-b.md`.
- **Timestamp:** 2026-07-03T17:55:19Z
- **Decision:** APPROVED
- **Checks performed:**
  - Steps 1–3: optional `isProbeFailed` appended after `isSignificantlyBehind` on the availability type with the planned rationale comment (error-code union untouched); service return type widened and the probe catch returns `{ isSignificantlyBehind: false, isProbeFailed: true }` with the replacement comment verbatim.
  - Step 4: `MithrilController.ts` carries no unstaged diff — pass-through via the existing `{ isEnabled, ...behindness }` spread confirmed; the two early returns intentionally do not set the flag.
  - Steps 5–6: store observable with rationale comment + `Boolean(availability.isProbeFailed)` in `_applyAvailability`, verbatim.
  - Steps 7–10: container passes `isProbeFailed`; Diagnostics Props/destructure updated; prop replacement removes the inner `isMithrilPartialSyncEnabled` re-check while the OUTER `{isMithrilPartialSyncEnabled && (` kill-switch gate is intact (re-read in place); remaining section props unchanged.
  - Steps 11–12: `shouldShowRecommendation` replaced by `isSignificantlyBehind`/`isProbeFailed` (old name now absent repo-wide, greps of source/ + storybook/ confirm); early `return null` replaced by the variant computation with the planned precedence (behind > availability-unknown > near-tip); CTA `disabled` still driven only by `isActionBlocked`; confirmation-precedes-start flow untouched; the `'Unable to start Mithril partial sync.'` fallback left for CAT-E.
  - Steps 13a–13c: `recommendationNearTip`/`recommendationUnknown` messages, exported `MithrilAvailabilityVariant`, required `variant` prop, and tooltip selection all verbatim; sole consumer (`MithrilPartialSyncSection`) passes the new prop.
  - Steps 14–16: EN + JA keys verbatim and alphabetically placed after `...Recommendation`; `defaultMessages.json` and `translations/messages.json` diffs contain exactly the two new entries each.
  - Steps 17–20: service spec degrade-test update + new local-read-rejection test, controller pass-through test, store apply/clear test, and the section spec changes (defaultProps swap, two visibility tests replacing the gated-off test, deep-link rename/re-key, tooltip test untouched) all match the plan.
  - Out-of-plan `DaedalusDiagnostics.spec.tsx`: accepted as a mechanical consequence — `defaultProps` gained the new required prop, and the one test asserting the removed hide-when-near-tip behavior was retitled and now asserts the CTA stays enabled after a near-tip rerender; the kill-switch hide test is untouched (re-read in place, boundary 10 guard preserved).
  - Vocabulary and hygiene: new user-facing strings say "Mithril Sync", no percentages, no user-visible "immutable" (code comments/test titles exempt); no task/finding IDs in comments or test titles; JA near-tip first sentence flagged per plan (no exact copy-table precedent, used exactly as planned).
  - No stray edits: unstaged diff covers exactly the reported files + this log; `.gitignore` (`.devcontainer` line) is the user's pre-existing in-flight edit, untouched and NOT staged; `.agent/skills/` untouched; no bootstrap files touched.
  - Implementer-reported runs (service 69, controller 11, store 36, section+diagnostics 29, bootstrap guard 5 suites/73, `yarn i18n:manage` exit 0, `yarn compile` clean) are plausible against the diff; the Verify phase re-runs everything.
- **Findings:** none.
- **Action:** staged the chunk's sixteen source/locale/catalog files plus this log.

---

## Implementation — CAT-B chunk 2 (T12/T31, T15, T21, T28; T2 record-only)

- **Label:** Implementation
- **Scope:** CAT-B chunk 2 — plan section 2 (Steps 21–31) of `task-ux-703-plan-cat-b.md`: start-rejection predicate (swallow only on resynced `failed`, session-flag reset on resynced `idle`), flag-comment update, recovery-action catch/log/resync for cancel/restart-normal/wipe-and-full-sync, App overlay retry wrap, behind-ness cache invalidation on chain-directory change, `Promise.all` join of the two probe reads, and the chunk's planned specs. T2 = no code change (checklist-only), recorded here.
- **Timestamp:** 2026-07-03T18:03:19Z
- **Outcome:** Completed. Every quoted anchor matched the working tree; all plan steps applied as written. No structural deviations; two of my hunks reflowed to prettier 2.1.2 output (see Deviations).
- **Files changed:**
  - `source/renderer/app/stores/MithrilPartialSyncStore.ts` (Steps 21–24 — post-resync predicate pinned to `failed`-swallow/`idle`-reset-inside-`runInAction`/rethrow-otherwise; flag declaration comment updated; cancel gains catch+log before its existing finally; restartNormally/wipeAndFullSync gain catch/log/finally-resync; `START_PENDING_STATUS` constant retained for the optimistic `_updateStatus`)
  - `source/renderer/app/App.tsx` (Step 25 — `logger` import after `translations`; `onRetry` wrapped so the start rethrow is caught and logged, other overlay props untouched)
  - `source/main/mithril/MithrilPartialSyncService.ts` (Steps 26, 28 — `_invalidateBehindnessCaches` comment extended + new `onChainDirectoryChanged()` delegator; the two probe reads joined via `Promise.all` with `_getCachedCertifiedEpoch()` left AFTER the joined await)
  - `source/main/mithril/MithrilController.ts` (Step 27 — `resetStartupGateOnDirectoryChange` drops the behind-ness caches via `_partialSyncService.onChainDirectoryChanged()` before the startup-gate reset; wiring guard verified: `source/main/utils/handleDiskSpace.ts` still calls `mithrilController.resetStartupGateOnDirectoryChange()`)
  - Specs per Steps 29–31: `MithrilPartialSyncStore.spec.ts` (idle-rethrow+re-arm test, working-status-rethrow test, cancel test flipped to resolves+logger assert, restart-normal/wipe rejection tests, delegation test count 2→4 with updated comment; the pre-existing absorb/rethrow tests verified still green), `MithrilPartialSyncService.spec.ts` (directory-change invalidation test, concurrency test — microtask flush proved reliable, no fallback needed), `MithrilController.spec.ts` (`mockOnChainDirectoryChanged` mock + factory entry, new `resetStartupGateOnDirectoryChange` describe incl. the `(controller as any)._startupGate` assertion, which typechecked fine)
- **Tests run:**
  - `yarn test:jest --testPathPattern "MithrilPartialSyncService"` — 1 suite, 71 tests green (re-run green after the prettier reflow of the concurrency test)
  - `yarn test:jest --testPathPattern "MithrilController"` — 1 suite, 12 tests green
  - `yarn test:jest --testPathPattern "MithrilPartialSyncStore"` — 1 suite, 40 tests green
  - Boundary-11 guard `yarn test:jest --testPathPattern "MithrilBootstrap"` — 5 suites, 73 tests green (no bootstrap file touched; App.tsx retry wrap only changes the overlay's `onRetry` binding)
  - `yarn compile` — clean (scss type generation succeeded this run; no typed-scss-modules workaround needed)
  - `prettier --check` on all seven touched files — my hunks clean; remaining warnings are the pre-existing drift documented below
- **Deviations:**
  - Prettier 2.1.2 reflow of two of my hunks, per the plan's "accept prettier's final line-breaking" allowance: the App.tsx `logger.warn('App: Mithril partial sync retry rejected', {...})` call broke to the multi-line-argument form, and the concurrency test's `jest.spyOn(...).mockImplementation(...)` chain collapsed to the single-line-chain form. Semantics identical to the plan snippets.
  - Remaining 2.1.2 warnings in `MithrilPartialSyncService.ts(.spec)`, `MithrilController.ts(.spec)`, and `MithrilPartialSyncStore.spec.ts` are pre-existing drift outside my hunks (spot-verified at HEAD via `git show HEAD:<file>` / `prettier --stdin-filepath --check`) — left untouched per the no-reformat-beyond-your-hunks rule. `MithrilController.ts` drift (e.g. the `StatusSender` field declarations) predates this chunk too.
- **T2 (record-only):** no source edit, no test — `mithrilPartialSyncThresholdImmutables = 20` and `_getBehindnessThresholdImmutables()` untouched; no renderer-side threshold added (boundary 4). Reviewer reply lives in `task-ux-703-pr-comment-checklist.md` (manual).
- **Notes:** No new user-facing strings in this chunk (no i18n work; `yarn i18n:manage` not needed). No task/thread IDs in comments or test titles — all new comments are plain rationale at surrounding density. Recovery actions still come strictly from backend `allowedRecoveryActions`; the catch/resync work adds no renderer inference or UI surface (DD-703-11 honored). Nothing staged/committed.

---

## Code Review — CAT-B chunk 2 round 1 (T12/T31, T15, T21, T28; T2 record-only)

- **Label:** Code Review
- **Scope:** CAT-B chunk 2 round 1 — plan section 2 (Steps 21–31) of `task-ux-703-plan-cat-b.md`.
- **Timestamp:** 2026-07-03T18:06:35Z
- **Decision:** APPROVED
- **Checks performed:**
  - Steps 21–22: post-resync predicate pinned exactly as planned — swallow ONLY on resynced `failed` (logger message string unchanged, so the pre-existing absorb test stays valid), session flag re-armed ONLY on resynced `idle` inside `runInAction` (already imported from mobx, line 1), every other resynced status rethrows via `toStartError`; `START_PENDING_STATUS` retained and still used by the optimistic `_updateStatus` (line 399); flag declaration comment replaced verbatim.
  - Steps 23–24: `cancelPartialSync` gains catch+log before its untouched finally/comment; `restartNormally` and `wipeAndFullSync` gain catch/log plus `finally { await this.syncStatus(); }` — verbatim; DD-703-11 invariant honored (catch/log/resync only, no new UI surface, recovery actions still sourced solely from backend `allowedRecoveryActions`).
  - Step 25: `logger` import inserted immediately after the `translations` import (`source/renderer/app/utils/logging.ts` exists); `onRetry` wrapped with catch+log, other overlay props untouched; the `logger.warn` argument reflow to multi-line is within the plan's explicit "accept prettier's final line-breaking" allowance — semantics identical.
  - Steps 26–27: `_invalidateBehindnessCaches` comment extended and `onChainDirectoryChanged()` delegator added verbatim; `MithrilController.resetStartupGateOnDirectoryChange` drops the caches before the startup-gate reset — hooked into the existing wiring, no new coordinator callbacks.
  - Step 28: the two probe reads joined via `Promise.all`; the `_getCachedCertifiedEpoch()` read remains AFTER the joined await (invariant honored, not reordered).
  - Step 29 a–e: absorb + pending-rethrow tests untouched; idle-rethrow+re-arm and working-status-rethrow tests added after the pending-rethrow test; cancel test flipped to `resolves.toBeUndefined()` with the `expect.objectContaining` logger assert; restart-normal/wipe rejection tests added; delegation test count 2→4 with the planned comment.
  - Step 30: directory-change invalidation test and concurrency test both inside `describe('getPartialSyncBehindness')` after the lifecycle-reset test (spec lines 2255/2281/2309); concurrency test kept (microtask flush reliable, fallback not needed); the `jest.spyOn(...).mockImplementation(...)` single-line-chain collapse is prettier-only, semantics identical.
  - Step 31: `mockOnChainDirectoryChanged` const + factory entry added in the planned positions; new `resetStartupGateOnDirectoryChange` describe placed after the `getPartialSyncAvailability` describe, including the `(controller as any)._startupGate` assertion (implementer reports it typechecked).
  - T2: record-only honored — `_getBehindnessThresholdImmutables()` and the threshold config appear only as untouched context in the diff; no renderer-side threshold added (boundary 4).
  - Hygiene: no task/finding IDs in any new comment or test title — all new comments are plain rationale at surrounding density; no new user-facing strings (no i18n work needed, correctly skipped); no bootstrap files touched (App.tsx change is the overlay `onRetry` binding only; bootstrap guard 5 suites/73 reported green — boundary 11).
  - No stray edits: the unstaged diff covers exactly the eight reported files plus nothing else; `.gitignore` unstaged change is the user's pre-existing in-flight edit, untouched and NOT staged; `.agent/skills/` untouched.
  - Implementer-reported runs (service 71, controller 12, store 40, bootstrap guard 5 suites/73, `yarn compile` clean, prettier clean on new hunks) are plausible against the diff; the Verify phase re-runs everything.
- **Findings:** none.
- **Action:** staged the chunk's seven source/spec files plus this log.

---

## Implementation — CAT-C chunk 1 (T13)

- **Label:** Implementation
- **Scope:** CAT-C chunk 1 — plan section 1 (Steps 1–13) of `task-ux-703-plan-cat-c.md`: finalize retry-once effect, finalize-failed error view with renderer-local retry action, finalize-failed copy EN+JA, two new overlay specs, storybook auto-play story, catalog regeneration.
- **Timestamp:** 2026-07-03T18:12:01Z
- **Outcome:** Completed. Every quoted anchor matched the working tree (constant/effect/swallowed-catch anchors exactly as the plan verified); all plan steps applied verbatim. No structural deviations.
- **Files changed:**
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts` (Step 1 — `partialSyncFinalizeFailed{Title,Hint,Retry}` descriptors inserted between `partialSyncFailedHint` and `partialSyncCancelledTitle`, `!!!` defaultMessage convention kept)
  - `source/renderer/app/i18n/locales/en-US.json` / `ja-JP.json` (Steps 2–3 — `loading.mithrilPartialSync.error.finalizeFailed.{hint,retry,title}` inserted in alphabetical position with polished copy; JA strings used verbatim from the plan's copy-table derivation — 後処理 for cleanup, 「…に失敗しました」/「…を完了できませんでした」/再試行/「…してください。」 patterns; no free translation, no residual uncertainty beyond that derivation)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` (Steps 4–9 — `useRef`/`useState` import; `FINALIZE_RETRY_DELAY_MS = 2000` with rationale comment; auto-finalize effect replaced with retry-once + disposed/unmount-guarded version (4000 ms trigger and cleanup semantics unchanged; no `.catch(() => {})` remains — grep-verified); `handleFinalizeRetry`; `isFinalizeFailureShown`/`showProgressView` gating with finalize-failed copy pair resolved locally (not in `partialSyncErrorCopy.ts`, untouched); `finalizeFailureActions` single primary action bypassing recovery-action membership; error view renders `error={null}` + `finalizeFailureActions` on the finalize frame)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` (Step 10 — two new tests appended before the describe close; all existing tests untouched)
  - `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx` (Step 11 — `onDismissCompleted` StoryProps entry, pass-through after the `{...baseProps}` spread, and the `'Completed - Finalize Failed (auto-plays)'` story with the rejecting handler)
  - Generated by Step 12 (`yarn i18n:manage`, not hand-edited): `source/renderer/app/i18n/locales/defaultMessages.json`, `translations/messages.json` (3 new descriptors each; polished en-US/ja-JP entries preserved, no `!!!` rewrite)
  - This log file.
- **Tests run:**
  - `yarn i18n:manage` — exit 0; 3 `finalizeFailed` keys present in both generated catalogs; locale files kept polished copy.
  - `node_modules/.bin/prettier --write` on the four touched source files, then `--check` — all clean; no reflow of the plan-verbatim snippets.
  - `yarn test:jest --testPathPattern "source/renderer/app/components/loading/mithril-bootstrap"` — 5 suites, 67 tests, all green (bootstrap regression gate `MithrilBootstrap.spec.tsx` included — boundary 11; `partialSyncErrorCopy.spec.ts` and the locale placeholder test included).
  - Verbose re-run of `MithrilPartialSyncOverlay.spec` — both new tests green (`retries a rejected finalize once...`, `shows the finalize-failed error view...`) plus the pre-existing gates `auto-fires finalize on the completed timeout with no Continue button`, `catches a rejecting onDismissCompleted from the auto-dismiss timer (no unhandled rejection)`, and `ships polished runtime strings without placeholder markers`.
  - No `.scss` touched — no typed-scss-modules regeneration needed; jest ran clean without the identity-obj-proxy sidecar.
- **Deviations:** none.
- **Notes:** `MithrilProgressView.tsx`/`MithrilErrorView.tsx` untouched (git status confirms — boundary 11 / CAT-G ownership); recovery-action membership untouched, the finalize retry is renderer-local like the defensive Quit (boundary 2); `error={null}` keeps raw backend text off the finalize frame (boundary 8); auto-finalize trigger unchanged (boundary 9, D9 + D-702a-1); no task/finding IDs in comments or test titles; user-facing copy says "Mithril Sync" only. The plan's Escalation 1 note stands as informational: today's store `dismissCompletedOverlay` never rejects, so the failure frame is contract-hardening of the typed `onDismissCompleted(): void | Promise<void>` seam — store intentionally not modified. Nothing staged/committed.

---

## Code Review — CAT-C chunk 1 round 1 (T13)

- **Label:** Code Review
- **Scope:** CAT-C chunk 1 round 1 — plan section 1 (Steps 1–13) of `task-ux-703-plan-cat-c.md`: finalize retry-once effect, finalize-failed error view with renderer-local retry, finalize-failed copy EN+JA, two overlay specs, storybook auto-play story, catalog regeneration.
- **Timestamp:** 2026-07-03T18:15:09Z
- **Decision:** APPROVED
- **Checks performed:**
  - Steps 1–3: the three `partialSyncFinalizeFailed{Title,Hint,Retry}` descriptors inserted verbatim between `partialSyncFailedHint` and `partialSyncCancelledTitle` with the `!!!` defaultMessage convention; EN and JA locale entries inserted at the exact alphabetical anchors with the plan's polished copy; JA strings byte-identical to the plan's copy-table derivation (後処理/「…に失敗しました」/「…を完了できませんでした」/再試行/「…してください。」) — no free translation, no residual uncertainty.
  - Steps 4–6: `useRef`/`useState` import extension, `FINALIZE_RETRY_DELAY_MS = 2000` with rationale comment, and the retry-once effect all verbatim; the 4000 ms auto-finalize trigger and cleanup semantics unchanged (boundary 9, D9 + D-702a-1); every catch path schedules the retry or sets visible state — no `.catch(() => {})` remains in the file; disposed/unmount guards and retry-timer cleanup present as planned.
  - Steps 7–9: `isFinalizeFailureShown` gated on `finalizeFailed && status === 'completed'`; `showProgressView` drives both the heading id and the view switch; finalize-failed copy pair resolved locally (`partialSyncErrorCopy.ts` untouched, per the plan's ownership note); `finalizeFailureActions` is a single primary retry action used only when `isFinalizeFailureShown`; `error={null}` on the finalize frame keeps raw backend text out (boundary 8); recovery-action membership still driven strictly by the backend booleans (boundary 2).
  - Step 10: both new tests appended verbatim before the describe close; all existing tests untouched (auto-fire gate, rejecting-onDismissCompleted gate, polished-strings locale scan); `act`/`fireEvent` already imported in the spec; test titles are plain behavior statements.
  - Step 11: StoryProps `onDismissCompleted` entry, pass-through placed after the `{...baseProps}` spread, and the `'Completed - Finalize Failed (auto-plays)'` story with the rejecting `action(...)` handler — verbatim; `action` already imported; rejections are all caught inside the component.
  - Step 12: generated-catalog diffs contain exactly the three new descriptors in each of `defaultMessages.json` and `translations/messages.json`; polished en-US/ja-JP entries preserved (no `!!!` rewrite).
  - Hygiene/boundaries: `MithrilProgressView.tsx`/`MithrilErrorView.tsx` untouched (boundary 11; bootstrap gate reported green — 5 suites, 67 tests); user-facing copy says "Mithril Sync" only — no "partial sync", no percentages, no "immutable" (descriptor `description` fields are translator-facing and follow the existing convention); no task/finding IDs in comments or test titles; store/`dismissCompletedOverlay` untouched per Escalation 1.
  - No stray edits: the unstaged diff covers exactly the eight reported source/generated files plus this log; `.gitignore` unstaged change is the user's pre-existing in-flight edit, untouched and NOT staged; `.agent/skills/` untouched.
  - Implementer-reported runs (`yarn i18n:manage` exit 0; bootstrap-folder jest 5 suites/67 green incl. both new tests and the three pre-existing gates; prettier clean on the four source files) are plausible against the diff; the Verify phase re-runs everything.
- **Findings:** none.
- **Action:** staged the chunk's eight files plus this log.

---

## Implementation — CAT-D chunk 1 (T7 + T18)

- **Label:** Implementation
- **Scope:** CAT-D chunk 1 — plan section 1 (Steps 1–16) of `task-ux-703-plan-cat-d.md`: preflight delta formula with measurement-failure fallback, managed-chain-path threading, `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` union member + copy map entry, disk-space copy EN+JA, five new service preflight tests + copy-resolver row, catalog regeneration, cheat-sheet/copy-table doc updates.
- **Timestamp:** 2026-07-03T18:23:14Z
- **Outcome:** Completed. All quoted anchors matched the working tree except one locale-file anchor shifted by CAT-C (see Deviations); all plan snippets applied verbatim.
- **Files changed:**
  - `source/main/mithril/MithrilPartialSyncService.ts` (Steps 1–3 — `PARTIAL_SYNC_DISK_MARGIN_FACTOR = 0.2` + derived `PARTIAL_SYNC_DISK_SAFETY_FACTOR = 1 + margin` with the rewritten rationale comment; `_assertSufficientDiskSpace(stagingRootPath, managedChainPath)` with delta formula `max(max(snapshot − chainDir, 0) + 0.2 × snapshot, DISK_SPACE_REQUIRED)`, whole-snapshot-bound fallback + warn log on local-size measurement failure, free-space fail-open unchanged; thrown message reworded to "Not enough free disk space on the chain storage volume for Mithril Sync." (the "Mithril partial sync" phrasing was still present as the plan expected — no earlier-section drift); call site threads `context.layoutResult.managedChainPath`)
  - `source/common/types/mithril-partial-sync.types.ts` (Step 4 — `'PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE'` appended to the union, append-only, no reordering)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts` (Step 5 — `partialSyncErrorInsufficientDiskSpace{Title,Hint}` descriptors inserted between `partialSyncErrorConversionFailedHint` and `partialSyncRetry`)
  - `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts` (Steps 6–7 — `INSUFFICIENT_DISK_SPACE` descriptor pair after `CONVERSION_FAILED`; `COPY_BY_CODE` entry appended; `COPY_BY_STAGE` untouched — no `preparing` entry, per the plan's deliberate omission)
  - `source/renderer/app/i18n/locales/en-US.json` / `ja-JP.json` (Steps 8–9 — `loading.mithrilPartialSync.error.insufficientDiskSpace.{hint,title}` with the plan's exact EN and JA strings; JA composed from copy-table patterns per the plan's derivation — **uncertainty flagged**, native-speaker pass may refine, no free translation applied)
  - Generated by Step 10 (`yarn i18n:manage`, not hand-edited): `source/renderer/app/i18n/locales/defaultMessages.json`, `translations/messages.json` (2 new descriptors each)
  - `source/main/mithril/MithrilPartialSyncService.spec.ts` (Steps 11–13 — `lstat: jest.fn()` added to the fs-extra mock; both existing disk-space tests pinned to a 0-byte `_getPathSizeBytes` stub; `mockSnapshotMetadata` helper + five new tests inside `describe('disk-space preflight', …)` verbatim: margin-dominant block, margin-exact pass, floor enforcement, measurement-failure fallback + warn assert, zero-local ≈ whole-snapshot bound)
  - `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.spec.ts` (Step 14 — `it.each` row mapping the new code to the new title id)
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-smoke-test-cheat-sheet.md` (Step 15 — Low-disk checklist item rewritten with the delta+margin formula)
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-ja-copy-table.md` (Step 16 — rows 56/57 appended; last pre-existing row was 55, so the plan's "currently 56 and 57" numbering held)
  - This log file.
- **Tests run:**
  - `yarn test:jest --testPathPattern 'source/main/mithril/MithrilPartialSyncService.spec.ts'` — 1 suite, 76 tests, all green (all pre-existing suites route through the lstat-degraded/stubbed local size unchanged; the five new preflight tests pass).
  - `yarn test:jest --testPathPattern 'partialSyncErrorCopy'` — 1 suite, 13 tests green including the new `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` row.
  - `yarn i18n:manage` — exit 0; no duplicate ids; new keys present in both generated catalogs; the new keys do not appear in either locale's untranslated list; locale files kept the exact Step 8/9 strings (i18n:check did not rewrite them — diff is +2 lines per locale).
  - `yarn compile` — tsc clean (proves the union ↔ `COPY_BY_CODE` exhaustiveness coupling).
  - No `.scss` touched — no typed-scss-modules regeneration needed; no jest scss sidecar needed.
  - Targeted `prettier --check` on the six edited source/spec files: four report drift, but the identical drift exists in their **staged** (pre-CAT-D) content — pre-existing prettier-3-style vs repo prettier 2.1.2 disagreement (e.g. `(nextStage ?? …)` parens at service line 649, the `COPY_BY_CODE` record wrapping, `({…}) as any` in the copy spec), none of it inside this chunk's hunks; left untouched per the no-reformat-beyond-edited-hunks rule.
- **Deviations:**
  - Steps 8–9 insertion anchor: the plan says insert immediately after `loading.mithrilPartialSync.error.failed.title`, but CAT-C had already landed `error.finalizeFailed.{hint,retry,title}` directly after it. Following the anchor literally would break the plan's stated invariant ("keys stay alphabetically sorted"), so the two keys were inserted after `error.finalizeFailed.title` instead (`finalizeFailed` < `insufficientDiskSpace` < `latestDrift`). Strings byte-identical to the plan; `i18n:check` confirmed the ordering by not rewriting either file.
- **Notes:** boundaries held — `allowedRecoveryActions` untouched, error object shape `{message, code, logPath, stage}` unchanged, all disk math in the main-process service, thrown message contains "Mithril Sync" and no "partial sync"/"immutable"/percentages (unit-asserted); no bootstrap files touched; internal `partialSync*` identifiers retained; no task/finding IDs in comments or test titles. Plan Escalation 2 note stands as informational for the reviewer: the preflight consumes the already-resolved `context.layoutResult.managedChainPath` rather than re-calling `getManagedChainPath()` (avoids re-forking a `checkDiskSpace` probe per start). JA translation uncertainty per plan Escalation 3 is flagged above. Nothing staged/committed.

---

## Code Review — CAT-D chunk 1 round 1 (T7 + T18)

- **Label:** Code Review
- **Scope:** CAT-D chunk 1 round 1 — plan section 1 (Steps 1–16) of `task-ux-703-plan-cat-d.md`: preflight delta formula + managed-chain-path threading, `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` union member + copy-map entry, disk-space copy EN+JA, service preflight tests + copy-resolver row, catalog regeneration, cheat-sheet/copy-table updates.
- **Timestamp:** 2026-07-03T18:26:35Z
- **Decision:** APPROVED
- **Checks performed:**
  - Steps 1–3: margin/safety constants with the rewritten rationale comment, the two-arg `_assertSufficientDiskSpace` with delta formula `max(max(snapshot − chainDir, 0) + 0.2 × snapshot, DISK_SPACE_REQUIRED)`, whole-snapshot-bound fallback + warn on local-size measurement failure, unchanged free-space fail-open, and the reworded thrown message — all byte-identical to the plan. Call site threads `context.layoutResult.managedChainPath` per plan Step 2 / Escalation 2 (no re-resolution, no extra `checkDiskSpace` fork). `requiredGb` derives from `requiredBytes` after the branch, so the reported figure tracks whichever formula ran.
  - `_getPathSizeBytes` confirmed pre-existing (`source/main/utils/chainStorageManager.ts:501`, interface in `chainStorageManagerShared.ts:119`) — no new du/size logic written, per the verification-grill correction.
  - Step 4: union member appended last, no reordering; service keeps its local `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE` const.
  - Steps 5–7: both message descriptors inserted between `partialSyncErrorConversionFailedHint` and `partialSyncRetry` verbatim; `INSUFFICIENT_DISK_SPACE` pair after `CONVERSION_FAILED`; `COPY_BY_CODE` entry appended; `COPY_BY_STAGE` untouched (deliberate `preparing` omission preserved).
  - Steps 8–9: EN/JA locale strings byte-identical to the plan. Anchor deviation accepted: CAT-C had landed `error.finalizeFailed.{hint,retry,title}` directly after the plan's `error.failed.title` anchor, so insertion after `error.finalizeFailed.title` is the only placement satisfying the plan's stated alphabetical-sort invariant (`finalizeFailed` < `insufficientDiskSpace` < `latestDrift`, `hint` < `title`) — verified in both locale diffs. JA uncertainty remains flagged per plan Escalation 3; no free translation.
  - Step 10: generated-catalog diffs contain exactly the two new descriptors each in `defaultMessages.json` and `translations/messages.json`; locale files not rewritten (+2 lines each).
  - Steps 11–13: `lstat: jest.fn()` added to the fs-extra mock; both pre-existing disk-space tests pinned to 0-byte `_getPathSizeBytes` stubs at the plan's exact insertion points; `mockSnapshotMetadata` helper + five new tests inside `describe('disk-space preflight', …)` verbatim (margin-dominant block incl. path-threading + vocabulary asserts, margin-exact pass, floor enforcement, measurement-failure fallback + warn assert, zero-local ≈ whole-snapshot bound). Test titles are plain behavior statements.
  - Step 14: `it.each` row added mapping the new code to `loading.mithrilPartialSync.error.insufficientDiskSpace.title`.
  - Steps 15–16: cheat-sheet Low-disk item rewritten with the delta+margin formula verbatim; copy-table rows 56/57 appended (last pre-existing row was 55 — numbering held).
  - Boundaries/hygiene: `allowedRecoveryActions` untouched; error object shape `{message, code, logPath, stage}` unchanged; all disk math main-process-side; thrown message and all new copy say "Mithril Sync" with no "partial sync"/"immutable"/percentages (unit-asserted; descriptor `description` fields are translator-facing per existing convention); no bootstrap files touched; internal `partialSync*` identifiers retained; no task/finding IDs in source comments or test titles.
  - No stray edits: unstaged diff covers exactly the twelve reported files plus this log; `.gitignore` unstaged change is the user's pre-existing in-flight edit, untouched and NOT staged; `.agent/skills/` untouched.
  - Implementer-reported runs (service spec 76/76 incl. five new preflight tests; copy-resolver 13/13; `yarn i18n:manage` exit 0 with no locale rewrite; `yarn compile` clean proving the union ↔ `COPY_BY_CODE` coupling) are plausible against the diff; the Verify phase re-runs everything.
- **Findings:** none.
- **Action:** staged the chunk's twelve files plus this log.

---

## Implementation — CAT-E chunk 1 (T16 + T19)

- **Label:** Implementation
- **Scope:** CAT-E chunk 1 — plan section 1 (Steps 1.1–1.13) of `task-ux-703-plan-cat-e.md`: seven stable error codes appended to the union, metadata-unavailable copy pair + technical-details header (EN+JA), copy-map entries for the new codes, service/coordinator rejection throws recoded (prose demoted to `logger.warn`), metadata stage errors gain `PARTIAL_SYNC_METADATA_UNAVAILABLE` (with "certified immutable" rewording), `MithrilErrorView` body from intl maps only, spec updates, catalog regeneration.
- **Timestamp:** 2026-07-03T18:38:58Z
- **Outcome:** Completed. All quoted anchors matched the working tree (CAT-D's union member and copy-map entry had landed exactly as the plan anticipated); all plan snippets applied verbatim. One spec assertion outside the plan's list had to follow Step 1.9's recode (see Deviations).
- **Files changed:**
  - `source/common/types/mithril-partial-sync.types.ts` (Step 1.1 — seven members appended after `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE`, append-only)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts` (Step 1.2 — `partialSyncErrorMetadataUnavailable{Title,Hint}` before `partialSyncRetry`; `errorDetailsHeader` before `progressDiskCheck`)
  - `source/renderer/app/i18n/locales/en-US.json` / `ja-JP.json` (Step 1.3 — `error.metadataUnavailable.{hint,title}` after `error.latestDrift.title`; `errorDetailsHeader` before `errorTitle`; plan's exact EN and JA strings)
  - `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts` (Step 1.4 — `METADATA_UNAVAILABLE` pair after `CONVERSION_FAILED`; seven `COPY_BY_CODE` entries appended, rejection codes → `FAILED` as defensive fallbacks; CAT-D's entry was present so no totality escalation)
  - `source/main/mithril/MithrilPartialSyncService.ts` (Steps 1.5–1.10 — five code constants; start re-entry, cancel-after-cutover, `assertStartAllowed` (×2), `_assertRecoveryActionAllowed` (×2) guards recoded with `logger.warn` prose; both `preparing` metadata stage errors carry `PARTIAL_SYNC_METADATA_UNAVAILABLE_CODE`, second message reworded to "latest certified range"; explicit no-change list honored — drift/exit-code/disk throws and `_buildError` untouched)
  - `source/main/utils/chainStorageCoordinator.ts` (Step 1.11 Edits A–E — three code constants replace `PARTIAL_SYNC_DISABLED_ERROR`; `_assertPartialSyncStartAllowed` both branches → `PARTIAL_SYNC_ALREADY_RUNNING` with warn logs; disabled throw → `PARTIAL_SYNC_DISABLED` with warn log; recovery-fallback throw → `PARTIAL_SYNC_LAYOUT_UNSUPPORTED` (pre-existing warn block kept); `getMithrilPartialSyncDisabledError` returns the code; no other consumer of the export — grep-verified; bootstrap-side guards untouched)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx` (Step 1.12 — `hasTechnicalDetails` boolean replaces raw `detailsHeader`; raw-message body `<p>` deleted; collapsible header is the localized `errorDetailsHeader`; collapsed-details body untouched)
  - `source/main/mithril/MithrilPartialSyncService.spec.ts` (Step 1.13 — both cancel-prose assertions → `'PARTIAL_SYNC_CANCEL_NOT_ALLOWED'`; retry-prose assertion → `'PARTIAL_SYNC_START_NOT_ALLOWED'`; plus one unplanned swap, see Deviations)
  - `source/main/utils/chainStorageCoordinator.spec.ts` (Step 1.13 — recovery-fallback assertion → `'PARTIAL_SYNC_LAYOUT_UNSUPPORTED'`; bootstrap-in-progress assertion → `'PARTIAL_SYNC_ALREADY_RUNNING'`; bootstrap-side assertions untouched)
  - `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.spec.ts` (Step 1.13 — `it.each` row for `PARTIAL_SYNC_METADATA_UNAVAILABLE` after the `PARTIAL_SYNC_CONVERSION_FAILED` row; new test mapping the six rejection codes to generic failed copy)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` (Step 1.13 — raw-message-not-visible + localized "Technical details" assertions appended inside the mapped-error-code test)
  - Generated by `yarn i18n:manage` (not hand-edited): `source/renderer/app/i18n/locales/defaultMessages.json`, `translations/messages.json` (exactly the three new descriptors each)
  - This log file.
- **Tests run:**
  - `yarn test:jest --testPathPattern "MithrilPartialSyncService|chainStorageCoordinator"` — 2 suites, 118 tests green (after the Deviation-1 assertion swap).
  - `yarn test:jest --testPathPattern "MithrilErrorView|MithrilPartialSyncOverlay|partialSyncErrorCopy|MithrilBootstrap"` — 7 suites, 106 tests green, including `MithrilBootstrap.spec.tsx` (boundary 11) and the new overlay/copy-resolver assertions.
  - `yarn i18n:manage` — exit 0; generated-catalog diffs contain exactly the three chunk-1 descriptors; locale files kept the exact Step 1.3 strings (no `!!!` seeded, no rewrite).
  - `yarn compile` — tsc clean (proves union ↔ `COPY_BY_CODE` totality with CAT-D's member present).
  - `yarn lint` — pre-existing warnings only, no errors.
  - Grep gates: no `T16|CAT-E|DD-703` in `source/` ts/tsx; no leftover `PARTIAL_SYNC_DISABLED_ERROR` references.
  - Targeted `prettier --check` on the seven edited source/spec files: all seven carry drift, but the identical drift exists in their staged (pre-chunk) content (e.g. the `COPY_BY_CODE` record wrapping and `COPY_BY_STAGE` `Partial<Record<…>>` shape pre-date this chunk); zero drift lines intersect this chunk's hunks (verified by diffing prettier output against both staged and working content); left untouched per the no-reformat-beyond-edited-hunks rule.
  - No `.scss` touched — no typed-scss-modules regeneration needed; no jest scss sidecar needed (renderer suites passed under the project config).
- **Deviations:**
  - Step 1.13 listed two service-spec assertions on the old recovery/retry prose, but a third test (`'still rejects the wipe from idle when no snapshot was adopted'`, service spec ~:1257) asserts the wipe-and-full-sync recovery prose recoded by Step 1.9. Replaced that message with `'PARTIAL_SYNC_RECOVERY_NOT_ALLOWED'` — mechanically identical to the plan's sibling swaps; no behavior change.
  - `yarn i18n:manage` run in chunk 1 (the plan schedules regeneration at Step 2.9, chunk 2) because the section-wide i18n convention requires regeneration for every new key; the catalogs picked up only this chunk's three descriptors, so Step 2.9 remains valid for chunk 2.
  - The two coordinator-spec `rejects.toThrow(…)` swaps collapsed to one line because the code string fits the print width (prettier-consistent; verified no drift added).
- **Notes:** boundaries held — recovery actions still come strictly from backend `allowedRecoveryActions` (Step 1.9 only recodes the guard's rejection); cancel-after-cutover behavior unchanged (message-only recode); rejection-path throws use code-as-message (survives Electron structured clone over IPC) with prose demoted to `logger.warn`; stage errors keep prose messages (confined to collapsed details after T19) and gain codes via `_createStageError`'s existing third argument; "certified immutable" wording removed from the thrown message; `MithrilProgressView.tsx` untouched; internal `partialSync*` identifiers retained; no task/finding IDs in source comments or test titles. JA uncertainty flagged per plan Escalation 4: 「技術的な詳細」 (no direct copy-table analogue) and the metadata-unavailable pair (patterned on table rows 43–44) — native review recommended at the next JA pass. JA copy-table doc rows for these keys are scheduled in chunk 2 (Step 2.10), not appended here. Nothing staged/committed.

---

## Code Review — CAT-E chunk 1 round 1 (T16 + T19)

- **Label:** Code Review
- **Scope:** CAT-E chunk 1 round 1 — plan section 1 (Steps 1.1–1.13) of `task-ux-703-plan-cat-e.md`: seven stable error codes in the union, metadata-unavailable copy pair + technical-details header (EN+JA), copy-map entries, service/coordinator rejection throws recoded with prose demoted to `logger.warn`, metadata stage errors gain a code, `MithrilErrorView` body from intl maps only, spec updates, catalog regeneration.
- **Timestamp:** 2026-07-03T18:43:38Z
- **Decision:** APPROVED
- **Checks performed:**
  - Step 1.1: seven members appended after `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` (CAT-D's, present as anticipated), append-only, byte-identical to the plan; D's member not re-added.
  - Steps 1.2–1.3: `partialSyncErrorMetadataUnavailable{Title,Hint}` inserted before `partialSyncRetry`, `errorDetailsHeader` before `progressDiskCheck`; EN/JA locale entries byte-identical to the plan at the plan's exact anchors (`metadataUnavailable.*` after `latestDrift.title`, `errorDetailsHeader` before `errorTitle`), alphabetical order preserved, no `!!!` in either locale.
  - Step 1.4: `METADATA_UNAVAILABLE` pair after `CONVERSION_FAILED`; all seven `COPY_BY_CODE` entries present with the six rejection codes mapped to `FAILED` as defensive fallbacks; CAT-D's copy entry present, so the plan's totality escalation did not trigger (implementer-reported `yarn compile` clean corroborates).
  - Steps 1.5–1.10: five code constants after `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE_CODE`; start re-entry, cancel-after-cutover, both `assertStartAllowed` branches, and both `_assertRecoveryActionAllowed` branches recoded byte-identically to the plan (warn prose + code-as-message); both `preparing` metadata stage errors carry `PARTIAL_SYNC_METADATA_UNAVAILABLE_CODE` via `_createStageError`'s existing third argument; "certified immutable" reworded to "certified range". Explicit no-change list honored: drift/exit-code/disk throws and `_buildError` untouched in the diff.
  - Step 1.11 Edits A–E: three coordinator code constants replace `PARTIAL_SYNC_DISABLED_ERROR`; both `_assertPartialSyncStartAllowed` branches → `PARTIAL_SYNC_ALREADY_RUNNING` with `[MITHRIL]` warn logs; disabled throw → `PARTIAL_SYNC_DISABLED` with warn log; recovery-fallback throw → `PARTIAL_SYNC_LAYOUT_UNSUPPORTED` with the pre-existing warn block kept; `getMithrilPartialSyncDisabledError` returns the code. Grep confirms zero `PARTIAL_SYNC_DISABLED_ERROR` references remain in `source/` or `tests/`. Bootstrap-side guards (`_assertBootstrapMutationAllowed` prose et al.) verified untouched.
  - Step 1.12: `hasTechnicalDetails` boolean replaces the raw `detailsHeader`; the raw-message body `<p>` deleted; collapsible header is the localized `errorDetailsHeader`; collapsed-details body (code div + `<pre>` message) untouched — the allowed raw-technical surface. Boundary 11: `MithrilBootstrap.spec.tsx` and `MithrilProgressView.tsx` untouched; no spec/story outside the overlay spec references the details header (grep-verified); bootstrap suite reported green.
  - Step 1.13: both cancel-prose assertions and the retry-prose assertion swapped to the codes; coordinator recovery-fallback and bootstrap-in-progress assertions swapped; bootstrap-side assertions untouched; copy-resolver `it.each` row + rejection-codes-to-generic-failed test verbatim; overlay spec gains the raw-message-not-visible and localized-header assertions at the plan's insertion point. Test titles are plain behavior statements.
  - Catalog regeneration: `defaultMessages.json` and `translations/messages.json` diffs contain exactly the three new descriptors each; locale files not rewritten (+3/+3 lines total across the two locales).
  - Deviations accepted: (1) the third service-spec swap (`'still rejects the wipe from idle when no snapshot was adopted'` → `'PARTIAL_SYNC_RECOVERY_NOT_ALLOWED'`) is a necessary consequence of Step 1.9's recode that Step 1.13 missed — mechanically identical to the plan's sibling swaps; (2) running `yarn i18n:manage` in chunk 1 follows the section's per-key i18n convention (header item d) and only picked up this chunk's three descriptors, leaving Step 2.9 valid; (3) the coordinator-spec one-line `rejects.toThrow` collapse is prettier-consistent formatting, no drift added.
  - Boundaries/hygiene: recovery actions still come strictly from backend `allowedRecoveryActions`; cancel-after-cutover behavior unchanged (message-only recode); no renderer-side inference or thresholds added; all new user-facing copy says "Mithril Sync"-domain vocabulary with no "partial sync"/"immutable"/percentages; no raw mithril-client JSON in copy; internal `partialSync*` identifiers retained; no task/finding IDs in source comments or test titles (grep-verified); JA strings byte-identical to the plan with Escalation-4 uncertainty carried forward in the implementer's entry.
  - No stray edits: unstaged diff covers exactly the fourteen reported code/catalog files plus this log; `.gitignore` unstaged change is the user's pre-existing in-flight edit, untouched and NOT staged; `.agent/skills/` untouched.
  - Implementer-reported runs (main-process 2 suites / 118 tests; renderer 7 suites / 106 tests incl. `MithrilBootstrap.spec.tsx`; `yarn i18n:manage` exit 0; `yarn compile` clean; lint pre-existing warnings only) are plausible against the diff; the Verify phase re-runs everything.
- **Findings:** none.
- **Action:** staged the chunk's fourteen files plus this log.

## Implementation — CAT-E chunk 2 (T1/T17 + T4 + T20/T32 + T22)

- **Label:** Implementation
- **Scope:** CAT-E chunk 2 — plan section 2 (Steps 2.1–2.10) of `task-ux-703-plan-cat-e.md`: stage-id labels for the real partial-sync service ids (T1/T17), "shut down" typo fix (T4), ICU plurals in the prompt and confirmation behind-lines (T20/T32), shared localized start-failure fallback (T22), locale JSON updates, test updates, catalog regeneration, JA copy-table doc update.
- **Timestamp:** 2026-07-03T18:54:22Z
- **Outcome:** Completed. All quoted anchors matched the working tree; all plan snippets applied verbatim. No escalations triggered.
- **Files changed:**
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts` (Step 2.1 — `partialSyncStage{Verifying,Converting,Installing}` + `partialSyncStartFailure` descriptors inserted between the tooltip descriptor and the closing `});`; the tooltip was still the last entry, as the plan's primary anchor expected)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx` (Step 2.2 Edits A–C — `verifying` added to `DOWNLOAD_SUB_IDS`; `converting`/`installing` added to `FINALIZE_SUB_IDS`; three `ITEM_ID_TO_MESSAGE` entries mapping the service stage ids to the new descriptors; bootstrap sub-item ids untouched)
  - `source/renderer/app/components/status/MithrilSyncProcessSummary.messages.ts` (Step 2.3 — `be shutdown.` → `be shut down.`, only that word pair)
  - `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx` (Step 2.4 — prompt body defaultMessage → `{epochs, plural, one {# epoch} other {# epochs}}`; no component-logic change)
  - `source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx` (Step 2.5 — confirmation behind defaultMessage → same ICU plural)
  - `source/renderer/app/i18n/locales/en-US.json` (Steps 2.3–2.6 — process-summary "shut down" fix; both ICU-plural values; `error.startFailure` after `error.stagedDbInvalid.title`; three `progress.stage*` keys after `progress.nodeStoppingTitle`; alphabetical order preserved)
  - `source/renderer/app/i18n/locales/ja-JP.json` (Steps 2.4–2.6 — both JA plural values collapsed to a single `other` branch; `error.startFailure` and three `progress.stage*` JA entries at the same anchors; plan's exact JA strings)
  - `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts` (Step 2.7 — `partialSyncStartFailureMessage: MessageDescriptor` exported after the resolver with the plan's rationale comment; the single shared start-failure fallback for CAT-F's helper to reuse)
  - `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` (Step 2.7 — import of the shared fallback after the confirmation import; the catch's `error.message`/hardcoded-prose fallback replaced by `this.context.intl.formatMessage(partialSyncStartFailureMessage)`; the prompt's analogous catch NOT touched, per the T22/T23 contract)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx` (Step 2.8 — two new tests feeding the real service ids (id === label) asserting localized labels render and raw ids do not, appended after the finalizing-substep test)
  - `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx` (Steps 2.3/2.8 — verbatim process-summary assertion gets the "shut down" fix; new singular "about 1 epoch behind" test after the known-figure test; existing 3-epochs assertions byte-identical)
  - `source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx` (Steps 2.3/2.8 — same "shut down" fix; new singular-epoch test after the epochs-only behind-ness test)
  - `source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx` (Step 2.8 — test retitled to `'keeps confirmation open and shows the localized start-failure fallback'`; asserts the localized fallback renders and the raw rejection message does not; the mock's rejection message intentionally left as-is)
  - Generated by `yarn i18n:manage` (not hand-edited): `source/renderer/app/i18n/locales/defaultMessages.json`, `translations/messages.json` (exactly the four new descriptors plus the T4/T20/T32 rewordings each; 52 diff lines fully accounted for)
  - `.agent/plans/mithril-partial-sync/mithril-partial-sync-ja-copy-table.md` (Step 2.10 — rows 2/18 updated to the ICU-plural EN/JA strings; row 12 EN cell "shut down" (JA unchanged); rows 58–64 appended: three stage labels, "Technical details", "Unable to start Mithril Sync.", metadata-unavailable title/hint, with the plan's screen-column values — this also covers the chunk-1 keys deferred here)
  - This log file.
- **Tests run:**
  - `yarn test:jest --testPathPattern "MithrilStepIndicator|MithrilErrorView|MithrilPartialSyncOverlay|partialSyncErrorCopy|MithrilBootstrap"` — 8 suites, 122 tests green (includes the two new step-indicator tests and `MithrilBootstrap.spec.tsx`, boundary 11).
  - `yarn test:jest --testPathPattern "MithrilPartialSyncSection|MithrilPartialSyncConfirmation|SyncingConnectingMithrilPrompt|MithrilProactivePromptContainer|DaedalusDiagnostics|MithrilPartialSyncStore"` — 7 suites, 103 tests green (includes both singular-epoch tests, the retitled fallback test, and the unchanged 3/8-epoch assertions under the plural `other` branch).
  - `yarn test:jest --testPathPattern "MithrilPartialSyncService|chainStorageCoordinator"` — 2 suites, 118 tests green (section-completion re-run; chunk 2 touched no main-process files).
  - `yarn i18n:manage` — exit 0; locale files kept the exact hand-written entries (no `!!!` seeded, no rewrite); catalog diffs contain exactly the chunk-2 descriptors/rewordings.
  - `yarn compile` — tsc clean. `yarn lint` — pre-existing warnings only, no errors.
  - Grep gates: no non-plural "epochs behind" in `en-US.json`; no "be shutdown" in the locale or descriptor; no "Unable to start Mithril partial sync" left in `MithrilPartialSyncSection.tsx`; no task/finding IDs in `source/` ts/tsx.
  - Targeted `prettier --check` on the eleven edited source/spec files: four flagged (`MithrilStepIndicator.tsx`, `partialSyncErrorCopy.ts`, `MithrilPartialSyncConfirmation.tsx`, `SyncingConnectingMithrilPrompt.spec.tsx`) — drift verified byte-identical between staged (pre-chunk-2) and working content via `prettier --stdin-filepath` on the staged blobs, so all of it pre-dates this chunk; zero drift added; left untouched per the no-reformat-beyond-edited-hunks rule.
  - No `.scss` touched — no typed-scss-modules regeneration needed; no jest scss sidecar needed (all renderer suites passed under the project config).
- **Deviations:** none.
- **Notes:** boundary 11 held — the bootstrap service never emits `verifying`/`converting`/`installing` item ids (its sub-item ids are `step-1..7`, `install-snapshot`, `cleanup`, `conversion`, all previously mapped), so the indicator edits cannot alter any bootstrap frame; `MithrilProgressView.tsx` untouched. T22/T23 contract held — `partialSyncStartFailureMessage` is the single exported start-failure fallback and the prompt's own catch (`'Unable to start Mithril sync.'`) is left for CAT-F. Vocabulary held — all new user-facing copy says "Mithril Sync", no percentages, no "immutable", no "partial sync"; internal `partialSync*` identifiers retained; no task/finding IDs in source comments or test titles. JA uncertainty flagged per plan Escalation 4: the three stage labels (patterned on copy-table rows 24/26 「〜しています...」) and 「Mithril同期を開始できませんでした。」 (patterned on rows 33/44 「〜できませんでした」) — native review recommended at the next JA pass. Nothing staged/committed.

## Code Review — CAT-E chunk 2 round 1 (T1/T17 + T4 + T20/T32 + T22)

- **Label:** Code Review
- **Scope:** CAT-E chunk 2 round 1 — plan section 2 (Steps 2.1–2.10) of `task-ux-703-plan-cat-e.md`: stage-id labels for the real service ids (T1/T17), "shut down" typo fix (T4), ICU plurals in prompt and confirmation behind-lines (T20/T32), shared localized start-failure fallback (T22), locale JSON updates, test updates, catalog regeneration, JA copy-table update.
- **Timestamp:** 2026-07-03T18:58:00Z
- **Decision:** APPROVED
- **Checks performed:**
  - Step 2.1: the four descriptors (`partialSyncStage{Verifying,Converting,Installing}`, `partialSyncStartFailure`) inserted byte-identically to the plan between the tooltip descriptor and the closing `});`.
  - Step 2.2 Edits A–C: `verifying` appended to `DOWNLOAD_SUB_IDS`, `converting`/`installing` to `FINALIZE_SUB_IDS`, three `ITEM_ID_TO_MESSAGE` entries added at the plan's anchor; all bootstrap ids (`step-1..7`, `install-snapshot`, `cleanup`, `conversion`) untouched. `STATUS_TO_STEP` already maps `verifying`→`downloading` and `converting`/`installing`→`finalizing`, corroborating the sub-id grouping rationale.
  - Step 2.3: `be shutdown.` → `be shut down.` applied in exactly four places — the descriptor, `en-US.json`, and the verbatim assertions in `SyncingConnectingMithrilPrompt.spec.tsx` and `MithrilPartialSyncConfirmation.spec.tsx`; JA entry unchanged; only that word pair changed.
  - Steps 2.4–2.5: both defaultMessages and both `en-US.json` values use `{epochs, plural, one {# epoch} other {# epochs}}`; both `ja-JP.json` values collapse to the single `other {#エポック}` branch, byte-identical to the plan; no component-logic changes.
  - Step 2.6: `error.startFailure` inserted after `error.stagedDbInvalid.title` and the three `progress.stage*` keys after `progress.nodeStoppingTitle` in both locales at the plan's exact anchors; alphabetical order preserved; JA strings byte-identical to the plan.
  - Step 2.7: `partialSyncStartFailureMessage: MessageDescriptor` exported after the resolver with the plan's rationale comment (`MessageDescriptor` already imported at the file top); section import placed after the confirmation import; the catch's `error.message`/hardcoded-prose fallback replaced by `this.context.intl.formatMessage(partialSyncStartFailureMessage)` (`contextTypes` with `intl` already declared); the prompt's analogous catch verified untouched (its diff contains only the Step 2.4 defaultMessage change).
  - Step 2.8: both new step-indicator tests feed the real service ids (id === label) and assert localized labels render while raw ids do not — byte-identical to the plan; singular-epoch tests added at the plan's insertion points in both prompt and confirmation specs; section spec retitled and now asserts the localized fallback renders and the raw rejection message does not, with the mock rejection left as-is; `MithrilPartialSyncStore.spec.ts`, `MithrilProactivePromptContainer.spec.tsx`, `DaedalusDiagnostics.spec.tsx`, and the prompt's start-rejection test verified unmodified in the working-tree diff. Test titles are plain behavior statements.
  - Step 2.9: catalog diffs are exactly 8 hunks — per file, the four new descriptors appended to the `MithrilBootstrap.messages.ts` descriptor block plus the T4/T20/T32 rewordings; no `!!!` seeded into either locale; locale files not rewritten.
  - Step 2.10: copy-table rows 2/18 updated to the ICU-plural EN/JA pairs, row 12 EN cell only; rows 58–64 appended covering the three stage labels, "Technical details", "Unable to start Mithril Sync.", and the metadata-unavailable pair with the plan's screen-column values.
  - Grep gates re-run independently: all four return no hits (no non-plural "epochs behind" in `en-US.json`; no "be shutdown"; no "Unable to start Mithril partial sync" in the section; no task/finding IDs in `source/` ts/tsx).
  - Boundaries/hygiene: no locked-boundary contact — the chunk is copy/label/test-only (no recovery-action, trigger, threshold, or IPC surface changes); boundary 11 held (bootstrap ids and `MithrilProgressView.tsx` untouched; bootstrap suite reported green); all new user-facing copy says "Mithril Sync" with no percentages/"immutable"/"partial sync" (the section spec's "Mithril partial sync is disabled..." string is a mock rejection asserted NOT to render); internal `partialSync*` identifiers retained.
  - No stray edits: unstaged diff covers exactly the fifteen reported code/locale/catalog files plus the two `.agent/plans` docs; `.gitignore`'s unstaged `.devcontainer` line is the user's pre-existing in-flight edit, untouched and NOT staged; `.agent/skills/` untouched.
  - Implementer-reported runs (renderer 8 suites / 122 tests and 7 suites / 103 tests incl. the new/retitled tests; main-process 2 suites / 118; `yarn i18n:manage` exit 0; `yarn compile` clean; lint pre-existing warnings only) are plausible against the diff; the Verify phase re-runs everything.
- **Findings:** none.
- **Action:** staged the chunk's sixteen files plus this log.

## Implementation — CAT-F chunk 1 (T23 + T24 + T25 + T29/T30 + T3)

- **Label:** Implementation
- **Scope:** CAT-F chunk 1 — plan section 1 (1.1–1.5) of `task-ux-703-plan-cat-f.md`: T23 shared error-message helper, T24 + T25 status-predicate dedup, T29/T30 dead observables, T3 scss backdrop mixin removal (+ `.scss.d.ts` regen).
- **Timestamp:** 2026-07-03T19:04:34Z
- **Outcome:** ESCALATED at step 1.1/4a (T23). The T23 helper design conflicts with what CAT-E actually landed for T22/T16; applying step 4a as written would (a) break the landed CAT-E spec test and (b) introduce a behavior change not among the plan's three documented deltas. Stopped without improvising; all partial edits reverted, working tree left byte-identical to the post-CAT-E staged state (only this log file changed).
- **Escalation detail:**
  - **CAT-F plan assumption (step 1.1, 4a, 7a):** the diagnostics section prefers the concrete extracted rejection message and falls back only when no usable message exists — `getMithrilStartErrorMessage` returns `extractMithrilErrorMessage(error) ?? intl.formatMessage(fallback)`; step 7a inserts a new test after an existing test named `'keeps confirmation open and shows concrete start failure'`.
  - **What CAT-E landed (working tree, chunk 2, approved):** `MithrilPartialSyncSection.startFromConfirmation` sets `startError: this.context.intl.formatMessage(partialSyncStartFailureMessage)` UNCONDITIONALLY (the concrete-message branch was deleted, not just the literal swapped); the export in `partialSyncErrorCopy.ts` carries the rationale comment "the renderer must never surface the raw rejection message"; the section spec test is named `'keeps confirmation open and shows the localized start-failure fallback'` and asserts the concrete rejection message (`'Mithril partial sync is disabled by launcher configuration.'`) is NOT in the document. The CAT-F plan's step-7a anchor test name does not exist.
  - **Root conflict (planner-level, not anchor drift):** CAT-E's T16 recoded all user-reachable service/coordinator rejection prose into stable machine codes (`PARTIAL_SYNC_DISABLED`, `PARTIAL_SYNC_ALREADY_RUNNING`, …) with prose demoted to `logger.warn` — CAT-E plan step 2.7 Edit B rationale: "rejection messages are now stable codes, not user copy — always show the localized fallback; CAT-F's follow-up helper maps specific codes to specific copy on top of this message". CAT-F's helper as specified does NOT map codes to copy — it surfaces the raw extracted message, so post-T16 it would render raw codes like `PARTIAL_SYNC_DISABLED` as user-visible copy (banned "partial sync" vocabulary included). The two approved plans hold contradictory T22/T23 contracts for the same seam.
  - **Also affected:** step 5 (prompt) applies mechanically (its ternary is untouched by CAT-E, per contract), but its existing spec test `'shows a start-error line when the start action rejects'` asserts concrete prose renders — post-T16 the same raw-code concern applies to that surface; resolution should decide the shared semantics for both surfaces, not per-component.
  - **Blocked steps:** 1.1 steps 2 (helper's `getMithrilStartErrorMessage` semantics), 4a/4c, 5, 6 (helper spec pins the disputed semantics), 7a/7b. Untouched-but-dependent: 1.1 step 3 (store `toMithrilStartError` imports the disputed helper file). Steps 1.2–1.5 (T24, T25, T29/T30, T3) are independent of the conflict and were NOT blocked, but per the stop-on-conflict instruction nothing was left applied.
  - **State on exit:** created-then-deleted `source/renderer/app/utils/mithrilErrorMessage.ts`; edits to `source/renderer/app/stores/MithrilPartialSyncStore.ts` (toStartError removal + T29/T30 deletions) reverted via `git restore --worktree` back to the staged (post-CAT-E) content. `git status` for code files matches the pre-chunk state.
  - **Resolution needed from the planner:** either (i) T23's component-facing helper resolves codes via the existing copy map / dedicated descriptors (per CAT-E's stated contract) and the prompt's existing concrete-message test is updated accordingly, or (ii) fallback-always is adopted at both catch sites and the helper shrinks to the store-facing `toMithrilStartError` (+ shared fallback usage), with step 6/7 specs rewritten to pin that. Both options change plan-specified tests — an implementer must not choose.
- **Files changed:** this log file only.
- **Tests run:** none (stopped before any independent step completed; no code changes remain to test).
- **Deviations:** none beyond the escalation itself — no plan step was adapted or improvised.
- **Notes:** T23 preflight (step 1) itself PASSED before the conflict surfaced: `partialSyncStartFailureMessage` is exported from `partialSyncErrorCopy.ts`, EN locale value is exactly `Unable to start Mithril Sync.` (`loading.mithrilPartialSync.error.startFailure`, JA 「Mithril同期を開始できませんでした。」). T24 precondition and remaining anchors were not re-verified after the stop; re-verify on re-run.

## Implementation — CAT-F chunk 1 re-run (T23 as AMENDED + T24 + T25 + T29/T30 + T3)

- **Label:** Implementation
- **Scope:** CAT-F chunk 1 — plan section 1 (1.1–1.5) of `task-ux-703-plan-cat-f.md` with the 2026-07-03 T23 Amendment (code-to-copy mapping): T23 shared error-message helper + additive `resolvePartialSyncErrorCopyByCode` lookup export, T24 blocking-node-start delegation, T25 overlay `PROGRESS_STATUSES` dedup, T29/T30 dead observables, T3 scss backdrop mixin removal (+ `.scss.d.ts` regen). Chunk 2 (T26/T27) untouched.
- **Timestamp:** 2026-07-03T19:43:42Z
- **Outcome:** Completed. All amended-plan anchors matched the working tree verbatim; every step applied as written. T23 preflight (Step 1a/1b/1c) re-verified and HELD: fallback export + invariant comment present, EN `Unable to start Mithril Sync.`, `COPY_BY_CODE` with `FAILED.title` → `Mithril Sync failed`, and the landed section test still stubs prose outside the code union. T24 pre-check HELD: inline array element-identical to `MITHRIL_PARTIAL_SYNC_WORKING_STATUSES` (same 9 statuses). T27-style caller checks not applicable (chunk 2).
- **Files changed:**
  - `source/renderer/app/utils/mithrilErrorMessage.ts` (NEW — `extractMithrilErrorMessage`, `getMithrilStartErrorMessage` with code→`title` copy mapping and shared-fallback otherwise, `toMithrilStartError`)
  - `source/renderer/app/utils/mithrilErrorMessage.spec.ts` (NEW — 11 tests per plan Step 7)
  - `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts` (Step 2 — additive `resolvePartialSyncErrorCopyByCode` export appended at EOF; descriptors, map entries, resolver, fallback export, and all comments byte-identical)
  - `source/renderer/app/stores/MithrilPartialSyncStore.ts` (Step 4 — local `toStartError` deleted, `throw toMithrilStartError(startError)`, import added; T29/T30 — `behindByImmutables` and `elapsedSeconds` observable declarations + their two write lines deleted; `backendElapsed`/`startedAt` derivation and `certifiedEpoch`/`isSignificantlyBehind` untouched)
  - `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` (Step 8c — two new wrap tests after the rethrow-pending test; T29/T30 Step 5 — exactly the four listed assertion lines deleted, payload inputs kept)
  - `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` (Steps 5a–5c — catch routes through `getMithrilStartErrorMessage`; now-unused `partialSyncStartFailureMessage` import deleted after file-local search confirmed no other use)
  - `source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx` (Step 8a — coded-rejection test ADDED directly after the landed fallback test; landed test UNMODIFIED, verified pure-addition in the diff)
  - `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx` (Steps 6a–6b — raw `error.message`/hardcoded fallback ternary replaced by the helper; `intl` contextTypes already declared)
  - `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx` (Step 8b — concrete-prose test REPLACED by the coded/un-coded pair asserting mapped copy renders, raw code/prose never render)
  - `source/common/types/mithril-partial-sync.types.ts` (T24 — inline 9-status array replaced by one-line delegation to `isMithrilPartialSyncWorkingStatus` with the plan's rationale comment)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` (T25 — `PROGRESS_STATUSES` deleted; `isProgressStatus = isMithrilPartialSyncWorkingStatus(status) || status === 'completed'`; separate value import added below the type-import block)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` (T24 Step 3 — second `it` added inside the existing blocking-node-start describe)
  - `source/renderer/app/App.spec.tsx` (T30 Step 6 — the two dead `elapsedSeconds` mock lines deleted)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss` (T3 — `@include overlay-backrop;` and the mixin `@import` deleted; nothing else)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss.d.ts` — regenerated via targeted `typed-scss-modules`; byte-identical to previous (diff empty), so no working-tree change
  - This log file (this entry)
- **Tests run:**
  - `yarn test:jest --testPathPattern "renderer/app/utils/mithrilErrorMessage"` — 1 suite, 11 tests green (helper 100% coverage)
  - `yarn test:jest --testPathPattern "partialSyncErrorCopy"` — 1 suite, 15 tests green (spec untouched, per plan)
  - `yarn test:jest --testPathPattern "MithrilPartialSyncStore"` — 1 suite, 42 tests green (40 + 2 new)
  - `yarn test:jest --testPathPattern "MithrilPartialSyncSection|SyncingConnectingMithrilPrompt|MithrilPartialSyncConfirmation"` — 3 suites, 40 tests green (re-run green after prettier reflow of my hunks); landed CAT-E fallback test passed UNMODIFIED (Step 1c contract)
  - `yarn test:jest --testPathPattern "MithrilPartialSyncOverlay"` — 1 suite, 19 tests green (18 + 1 new T24 test; T25 routing identical)
  - Boundary-11 guard `yarn test:jest --testPathPattern "MithrilBootstrap"` — 5 suites, 73 tests green; T3 `.scss.d.ts` regen byte-identical (no class-name churn, completed frame untouched)
  - `yarn test:jest --testPathPattern "App.spec"` — 1 of 2 tests green; the FAILING test is PRE-EXISTING at the staged baseline (see Deviations)
  - `yarn compile` — clean (full scss precompile succeeded natively under Node 24)
  - `eslint` on all touched files — 0 errors; all warnings verified pre-existing via `git show :<file> | eslint --stdin` spot-check
  - `prettier --check` on all touched files — clean except `partialSyncErrorCopy.ts` and `MithrilPartialSyncStore.spec.ts`, whose failures are pre-existing drift OUTSIDE my hunks (staged blobs fail identically via `--stdin-filepath`; my appended/added hunks produce no prettier diff)
  - Acceptance greps: `grep -rn "Unable to start Mithril" source/renderer --include="*.ts*"` hits only the CAT-E message definition + locale files + the two spec literals; no `PROGRESS_STATUSES`, no `behindByImmutables` in the store, no `overlay-backrop` in `MithrilBootstrap.scss`
- **Deviations:**
  - **Pre-existing `App.spec.tsx` failure (NOT caused by this chunk):** `'mounts the partial sync overlay and forwards all recovery callbacks'` fails on the `onRetry` identity assertion (`onRetry: stores.mithrilPartialSync.startPartialSync` vs the inline wrapper CAT-B chunk 2 Step 25 landed in `App.tsx`). Proven pre-existing: restoring the staged `App.spec.tsx` (with the `elapsedSeconds` lines intact) against the staged `App.tsx` fails byte-identically; CAT-B chunk 2's entry above lists the `onRetry` wrap but did not update or run `App.spec`. My T30 change to this file is only the two dead `elapsedSeconds` mock-key deletions, unrelated to the failing assertion. Fixing it would touch CAT-B-owned assertions outside this chunk's steps — left for the CAT-B owner/orchestrator.
  - Prettier 2.1.2 reflow of my own hunks only (fixpoint verified by double-format): the plan-quoted `expect(screen.queryByText('PARTIAL_SYNC_DISABLED')).not.toBeInTheDocument()` collapsed to one line in both component specs; the prompt spec's `expect(logger.warn)` receiver broke to multi-line; the helper spec's double-cast became `({ formatMessage } as unknown) as Intl`. Semantics identical to the plan snippets.
- **Notes:** Zero new i18n messages (no locale/catalog edits, `yarn i18n:manage` not needed, per plan). No task/thread IDs in source comments or test titles — new comments are the plan's plain rationale text. Boundaries held: raw codes/prose never render (both specs assert absence); vocabulary clean; `.gitignore` and `.agent/skills/` untouched; nothing staged or committed.

## Code Review — CAT-F chunk 1 round 1

- **Label:** Code Review
- **Scope:** CAT-F chunk 1 round 1 — plan section 1 (1.1–1.5) of `task-ux-703-plan-cat-f.md` with the 2026-07-03 T23 Amendment: T23 shared error-message helper (code-to-copy mapping), T24 blocking-node-start delegation, T25 overlay `PROGRESS_STATUSES` dedup, T29/T30 dead observables, T3 scss backdrop mixin removal (+ `.scss.d.ts` regen).
- **Timestamp:** 2026-07-03T19:49:16Z
- **Decision:** APPROVED
- **Checks performed:**
  - Step 1 preflight independently re-verified: `partialSyncStartFailureMessage` export with the two-line invariant comment intact; `COPY_BY_CODE` present with `PARTIAL_SYNC_DISABLED` → `FAILED` (`partialSyncFailedTitle`, id `loading.mithrilPartialSync.error.failed.title`, EN `Mithril Sync failed`) and `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` → `partialSyncErrorInsufficientDiskSpaceTitle`; the landed CAT-E section test still stubs prose outside the code union (`'Mithril partial sync is disabled by launcher configuration.'`).
  - Step 2: `partialSyncErrorCopy.ts` unstaged diff is exactly the 8-line additive `resolvePartialSyncErrorCopyByCode` export appended after the fallback export — descriptors, map entries, resolver, and every comment byte-identical; no import changes; `partialSyncErrorCopy.spec.ts` untouched.
  - Step 3: `mithrilErrorMessage.ts` matches the amended plan byte-for-byte — `extractMithrilErrorMessage` (Error / `{ message }` / null), `getMithrilStartErrorMessage` (trimmed code → `copy.title`, otherwise shared fallback, never raw text), `toMithrilStartError` (Error pass-through, else wrap extracted-or-empty); comments are plain rationale.
  - Steps 4a–4c: local `toStartError` (with its banned `'Unable to start Mithril partial sync.'` literal) deleted; `throw toMithrilStartError(startError)`; import placed below the logging import. No `toStartError` identifier remains in the store.
  - Steps 5a–5c / 6a–6b: both catch seams now set `startError: getMithrilStartErrorMessage(error, this.context.intl)`; the section's now-unused `partialSyncStartFailureMessage` import deleted (zero remaining references verified); the prompt's raw `error.message`/hardcoded non-intl fallback ternary gone; both components carry `intl` in `contextTypes`.
  - Steps 7/8: helper spec matches the plan (11 tests; re-run independently: 1 suite, 11 passed, 100% coverage); section spec is a pure addition directly after the landed fallback test (diff has zero deleted content lines — the CAT-E test is UNMODIFIED, honoring the Step 1c contract); prompt spec's concrete-prose test replaced by the coded/un-coded pair asserting mapped copy renders and raw code/prose never do; store spec gained the two wrap tests after the rethrow-pending test with its payload shape.
  - T24: pre-check independently re-verified (delegation target `MITHRIL_PARTIAL_SYNC_WORKING_STATUSES` is exactly the same 9 statuses the deleted inline array carried); body is the one-line delegation with the plan's rationale comment; new overlay-spec predicate test added inside the existing describe, byte-identical to the plan.
  - T25: `PROGRESS_STATUSES` constant deleted; `isProgressStatus = isMithrilPartialSyncWorkingStatus(status) || status === 'completed'` (set-identical to the deleted array); separate value import added below the type-import block; `MithrilPartialSyncStatus` type import still consumed by Props.
  - T29/T30: both observable declarations and both write lines deleted; `backendElapsed`/`startedAt` derivation and `isPartialSyncEnabled`/`isSignificantlyBehind`/`certifiedEpoch` handling untouched; exactly the four listed spec assertion lines deleted (payload inputs kept); `App.spec.tsx` lost only the two dead `elapsedSeconds` mock keys. No `behindByImmutables` identifier remains in the store.
  - T3: scss diff is exactly the two lines (`@import` + `@include overlay-backrop;`); `.backdrop` stays opaque so the removal is a visual no-op; regenerated `MithrilBootstrap.scss.d.ts` exports `backdrop`/`card`/`component`/`content` (content-identical; the file is gitignored via the `*.scss.d.ts` rule, so there is nothing to stage); boundary-11 bootstrap suites reported green (5 suites / 73 tests).
  - Acceptance greps re-run independently: `"Unable to start Mithril"` hits only the CAT-E descriptor + the two spec literals in renderer ts/tsx; no `PROGRESS_STATUSES`; no `overlay-backrop` and no mixin `@import` in `MithrilBootstrap.scss`.
  - Boundaries/vocabulary/hygiene: no locked-boundary contact (dedup/dead-code/helper routing only; recovery actions, triggers, thresholds, IPC payloads, and networks untouched); raw codes are never rendered as copy — both component specs assert `PARTIAL_SYNC_DISABLED` absent from the document; zero new i18n messages (no locale/catalog edits, per plan); no task/finding IDs in source comments or test titles (new comments are the plan's plain rationale text; diff ID matches are confined to this log); no reformatting beyond edited hunks; `.gitignore` and `.agent/skills/` untouched and left unstaged; unstaged code diff covers exactly the fourteen reported code files plus the two new helper files and this log.
  - Implementer-reported runs (store 42/42, seam suites 40/40, overlay 19/19, bootstrap 73/73, `yarn compile` clean, eslint 0 errors, prettier failures confined to pre-existing drift outside the chunk's hunks) are plausible against the diff; the Verify phase re-runs everything.
- **Findings (non-blocking, not owned by this chunk):**
  - The `App.spec.tsx` failure (`'mounts the partial sync overlay and forwards all recovery callbacks'`, `onRetry` identity assertion) is confirmed pre-existing: `App.tsx` carries CAT-B chunk 2's inline `onRetry` wrapper while the spec (untouched by this chunk on that line) still asserts identity with `stores.mithrilPartialSync.startPartialSync`. This chunk's two mock-key deletions are unrelated. Needs the CAT-B owner/orchestrator before the Verify phase treats `App.spec` as a gate.
  - Disclosed prettier 2.1.2 reflow of the chunk's own hunks (spec expect-line collapses/breaks, helper-spec cast form) — semantically identical to the plan snippets; accepted.
- **Action:** staged the chunk's fourteen code files (two new untracked helper files included) plus this log.

## Implementation — CAT-F chunk 2 (T26 + T27)

- **Label:** Implementation
- **Scope:** CAT-F chunk 2 — plan section 2 (2.1–2.2) of `task-ux-703-plan-cat-f.md`: T26 shared spawn helper in `mithrilCommandRunner.ts` (detachment + POSIX rationale comment preserved), T27 snapshot list/show delegation to the raw metadata pipeline in `MithrilPartialSyncService.ts` (+ new pipeline spec).
- **Timestamp:** 2026-07-03T19:56:06Z
- **Outcome:** Completed. All quoted anchors matched the working tree verbatim; all plan snippets applied as written. Escalation-3 pre-check re-verified and HELD: no production caller of `MithrilPartialSyncService.listSnapshots`/`.showSnapshot` (grep across `source/` excluding specs — the live `listSnapshots` route is `mithrilBootstrapChannel` → `MithrilController` → `chainStorageCoordinator` → `MithrilBootstrapService.listSnapshots`; the only `.showSnapshot(` production hit is `MithrilBootstrapService` calling its own method), so the documented drop/throw delta stays inert. No escalations triggered.
- **Files changed:**
  - `source/main/mithril/mithrilCommandRunner.ts` (T26 Steps 1–3 — `SpawnMithrilChildParams` type + module-private `spawnMithrilChild` inserted between `attachLogStream` and `runBinary`, per-plan rationale comment; `runBinary`/`runCommand` bodies replaced by the plan's delegating versions. Exactly ONE `spawn(` site remains; the 4-line POSIX process-group rationale comment appears once, verbatim, above it; `detached: !environment.isWindows` unchanged; `runCommand` still prefixes `['--origin-tag', 'DAEDALUS', ...args]` and deliberately does NOT forward `stdinInput`; log-stream open + `onLogStream` still fire before the `buildMithrilEnv` await in `runCommand`)
  - `source/main/mithril/MithrilPartialSyncService.ts` (T27 Steps 1–3 — `listSnapshots` delegates to `_listSnapshotsRaw` mapping `({ snapshot }) => snapshot`; `showSnapshot` delegates to `_showSnapshotRaw` (its digest guard + parse-shape guard already cover the old checks); `_listSnapshotsRaw`/`_showSnapshotRaw` untouched; `normalizeSnapshotItem` removed from the `mithrilSnapshotMetadata` import after a file-local search confirmed the two deleted method bodies were its only remaining uses)
  - `source/main/mithril/MithrilPartialSyncService.spec.ts` (T27 Step 4 — plan's pipeline test added directly after `'uses a distinct partial sync log file when running Mithril commands'`, same `runCommandMock` fixture, byte-identical to the plan snippet)
  - This log file (this entry)
- **Tests run:**
  - `yarn test:jest --testPathPattern "mithrilCommandRunner"` — 1 suite, 21 tests green, spec UNMODIFIED per T26 Step 4 (both detached-POSIX tests, both Windows non-detached tests, origin-tag prefix, binary-path resolution, un-truncated stdout all pass against the extracted helper)
  - `yarn test:jest --testPathPattern "MithrilPartialSyncService"` — 1 suite, 77 tests green (76 + 1 new pipeline test); existing log-file-name spec green unmodified
  - `yarn compile` — clean (full scss precompile succeeded natively under Node 24; no scss touched by this chunk)
  - `eslint` on the three touched files — 0 errors; runner warnings are the pre-existing classes (function-type `chunk`/`child`/`logStream` params in type declarations, `default-param-last` on the unchanged exported signatures); the new `SpawnMithrilChildParams` callbacks carry the same benign no-unused-vars class as the pre-existing `RunCommandOptions` ones
  - `prettier --check` on the three touched files — all three fail, but ALL drift is pre-existing OUTSIDE this chunk's hunks: staged blobs fail identically via `git show :<file> | prettier --stdin-filepath <file> --check`, and the region-by-region diff comparison (working tree vs staged blob) shows the same regions (runner `normalizeSpawnEnv` reduce at ~81–85; service lines ~665 and ~999–1000; spec lines ~94–102 and two `runCommandMock.mockImplementation` regions) shifted only by this chunk's line-count deltas; my hunks produce zero new prettier diff
  - Acceptance greps: exactly one `spawn(` call site in `mithrilCommandRunner.ts`; `'cardano-db', 'snapshot'` `_runCommand` arrays now exist only inside `_listSnapshotsRaw`/`_showSnapshotRaw`; no `normalizeSnapshotItem` reference remains in the service
- **Deviations:** None. All snippets applied verbatim; anchors matched without relocation (the known-benign T26 full-pipeline-width drift documented in plan Escalation 5 is exactly what the plan's Steps 1–3 already encode).
- **Notes:** Zero new i18n messages, zero user-facing copy, zero renderer contact (main-process only). No task/thread IDs in source comments or test titles — new comments are the plan's plain rationale text. Nothing staged or committed; `.gitignore` and `.agent/skills/` untouched. Known pre-existing `App.spec.tsx` failure (CAT-B `onRetry` wrapper) is untouched by this chunk.

## Code Review — CAT-F chunk 2 round 1

- **Label:** Code Review
- **Scope:** CAT-F chunk 2 round 1 — plan section 2 (2.1–2.2) of `task-ux-703-plan-cat-f.md`: T26 shared spawn helper in `mithrilCommandRunner.ts`, T27 snapshot list/show delegation to the raw metadata pipeline in `MithrilPartialSyncService.ts` (+ new pipeline spec).
- **Timestamp:** 2026-07-03T19:59:13Z
- **Decision:** APPROVED
- **Checks performed:**
  - T26 Step 1: `SpawnMithrilChildParams` type and module-private `spawnMithrilChild` sit between `attachLogStream` and `runBinary`, byte-identical to the plan snippet including the three-line shared-pipeline rationale comment; `WriteStream` type import already present.
  - T26 spawn invariants independently re-verified on the working tree: exactly ONE `spawn(` call site remains (line 173); the 4-line POSIX process-group rationale comment appears once, verbatim, directly above it; `detached: !environment.isWindows` unchanged (DD-703-6 invariant held); the child is not `unref()`'d and stdio stays piped.
  - T26 Steps 2–3: `runBinary`/`runCommand` bodies are the plan's delegating versions with signatures unchanged; `runBinary` forwards `stdinInput`, `runCommand` deliberately does NOT; `runCommand` still prefixes `['--origin-tag', 'DAEDALUS', ...args]` and passes `binaryName: 'mithril-client'` (helper's `.exe` + `DAEDALUS_INSTALL_DIRECTORY` join reproduces the old resolution); log-stream open + `onLogStream` still precede the `buildMithrilEnv` await; the old `// Resolve mithril-client binary path` comment removed with its code.
  - T26 Step 4: `mithrilCommandRunner.spec.ts` UNMODIFIED (absent from git status entirely); implementer reports 21/21 green including both detached-POSIX, both Windows non-detached, origin-tag, and path-resolution tests — plausible against the diff.
  - T27 Steps 1–2: `listSnapshots` and `showSnapshot` replaced byte-identically with the plan's delegations (`_listSnapshotsRaw` mapped through `({ snapshot }) => snapshot`; `_showSnapshotRaw` shared); no `_runCommand(['cardano-db','snapshot'…])` + `_safeJsonParse` block remains in either public method.
  - T27 Step 3: `_listSnapshotsRaw`/`_showSnapshotRaw` untouched by the unstaged diff (digest guard + parse-shape guard confirmed in place at lines 1115/1122); `normalizeSnapshotItem` removed from the `mithrilSnapshotMetadata` import — file-local grep confirms zero remaining references.
  - T27 Step 4: new spec `'serves snapshot list and show reads from the shared metadata pipeline'` inserted directly after `'uses a distinct partial sync log file when running Mithril commands'`, byte-identical to the plan snippet, same `runCommandMock` fixture; test title carries no task/finding IDs.
  - Escalation-3 pre-check independently re-run: production `.listSnapshots(`/`.showSnapshot(` hits are only `chainStorageCoordinator` → `MithrilBootstrapService.listSnapshots`, `mithrilBootstrapChannel` → `MithrilController` → coordinator, and `MithrilBootstrapService` calling its own `showSnapshot` — none resolve to `MithrilPartialSyncService`, so the documented drop/throw delta stays inert.
  - Boundaries/vocabulary/hygiene: main-process refactor only — zero user-facing copy, zero i18n changes (per plan), no locked-boundary contact (recovery actions, triggers, thresholds, IPC payloads, networks, bootstrap flow all untouched; `MithrilBootstrapService`'s own pipeline not modified); no task/thread IDs in source comments; unstaged code diff covers exactly the three reported code files plus this log; `.gitignore` and `.agent/skills/` untouched and left unstaged.
  - Implementer-reported runs (runner 21/21, service 77/77 incl. the new pipeline test, `yarn compile` clean, eslint 0 errors, prettier failures confined to pre-existing drift outside the chunk's hunks with staged blobs failing identically) are plausible against the diff; the Verify phase re-runs everything.
- **Findings:** None blocking. Disclosed pre-existing prettier 2.1.2 drift in all three files (known oscillation pair + pre-existing service/spec regions) is outside this chunk's hunks — accepted, consistent with the chunk-1 precedent.
- **Action:** staged the chunk's three code files plus this log.

## Implementation — CAT-G chunk 1 (mechanical cleanups N2, N3, N5, N6, N8, N9)

- **Label:** Implementation
- **Scope:** CAT-G chunk 1 — plan section 1 (Steps 1.1–1.17) of `task-ux-703-plan-cat-g.md`: N2 dead epoch paths, N3 coordinator boolean getter, N5 shared idle-status factory, N6 static `runBinary` import, N8 no-op terminal filter removal, N9 story-constant hoist. N1 recorded as declined no-op (DD-703-13, checklist owned by the orchestrator); N7 out of scope (absorbed by CAT-B).
- **Timestamp:** 2026-07-03T20:09:53Z
- **Outcome:** Completed. Every quoted anchor matched the working tree verbatim (controller consumed only `isEnabled` from the coordinator call — the Step 1.5 escalation trigger did not fire; no stray `DEFAULT_STATUS` references beyond the three planned literals; the `await import('./mithrilCommandRunner')` was still present so N6 applied as written). No escalations triggered.
- **Files changed:**
  - `source/main/mithril/mithrilSnapshotMetadata.ts` (N2 — `extractCertifiedEpoch` trimmed to `['beacon','epoch']` + `['cardano_db_beacon','epoch']`; comment no longer declares dead paths; sibling immutable-number path list untouched)
  - `source/main/mithril/mithrilSnapshotMetadata.spec.ts` (N2 — bare-epoch preference test replaced by the plan's ignores-bare-epoch/non-beacon-spellings test)
  - `source/main/utils/chainStorageCoordinator.ts` (N3 — `getPartialSyncAvailability()` fabricated shape replaced by `isPartialSyncEnabled(): boolean`; now-unused `MithrilPartialSyncAvailability` type import removed; `PARTIAL_SYNC_DISABLED` seam untouched)
  - `source/main/utils/chainStorageCoordinator.spec.ts` (N3 — new boolean-getter test inserted before the accessors test)
  - `source/main/mithril/MithrilController.ts` (N3, N5 — coordinator call swapped to the boolean getter; `DEFAULT_PARTIAL_SYNC_STATUS` deleted; `_partialSyncStatus` initialized via `makeIdlePartialSyncStatus()`; value import extended; `DEFAULT_BOOTSTRAP_STATUS` untouched)
  - `source/main/mithril/MithrilController.spec.ts` (N3 — four mechanical mock renames to `mockIsPartialSyncEnabled` returning booleans; no `resolves.toEqual` payload changed)
  - `source/common/types/mithril-partial-sync.types.ts` (N5, N8 — `makeIdlePartialSyncStatus` factory added after the snapshot type with the plan's rationale comment; overlay-status list is now the plain concat of working + terminal arrays)
  - `source/common/types/mithril-partial-sync.types.spec.ts` (NEW — N5/N8 spec, byte-identical to the plan snippet: factory shape + fresh nested references; overlay membership for all 12 working/terminal statuses, never idle)
  - `source/main/mithril/MithrilPartialSyncService.ts` (N5, N6 — `DEFAULT_STATUS` deleted, `_status` factory-initialized, value import added below the types `import type` block; `runBinary` joined the static `runCommand` import and the dynamic `await import` removed; `_runBinary` stays async; the `_updateStatus({ status: 'idle', … , logPath: undefined })` reset payload is a distinct update-call argument, not a `DEFAULT_STATUS` reference, and was deliberately left alone per Step 1.10d)
  - `source/renderer/app/stores/MithrilPartialSyncStore.ts` (N5 — local `DEFAULT_STATUS` const kept but factory-sourced; value import extended)
  - `storybook/stories/loading/_support/mithrilFixtures.ts` (N9 — `snapshotFilesTotal = 980` exported after `snapshotSize`)
  - `storybook/stories/loading/mithril/MithrilProgressView.stories.tsx` (N9 — fixture import added alphabetically; local `SNAPSHOT_FILES_TOTAL` deleted; all four usages renamed)
  - `storybook/stories/loading/mithril/MithrilBootstrap.stories.tsx` (N9 — same treatment; both usages renamed)
  - This log file (this entry)
- **Tests run:**
  - `yarn test:jest --testPathPattern "(mithrilSnapshotMetadata|chainStorageCoordinator|MithrilController|mithril-partial-sync.types|MithrilPartialSyncService|MithrilPartialSyncStore)"` — 6 suites, 186 tests green (includes the new types spec and the new coordinator boolean-getter test; service conversion-tracking suite green through the statically imported `runBinary` mock)
  - `yarn test:jest --testPathPattern "(MithrilBootstrap|MithrilProgressView|MithrilPartialSyncOverlay)"` — 7 suites, 103 tests green (boundary-11 safety net for the shared types-module change; all bootstrap specs untouched and green)
  - `yarn compile` — clean (scss precompile succeeded natively; no scss touched by this chunk)
  - `yarn lint` — exit 0 (warnings are the pre-existing repo-wide noise); targeted eslint on all 13 touched files — 0 errors (warnings are pre-existing classes, e.g. mobx decorator-import false positives)
  - `prettier --check` on all touched files — remaining failures confined to pre-existing drift: staged blobs of `chainStorageCoordinator.ts/.spec`, `MithrilController.ts/.spec`, `MithrilPartialSyncService.ts` fail identically via `git show :<file> | prettier --stdin-filepath <file> --check`, and region-count comparison (working tree vs staged blob) shows zero new drift regions from this chunk's hunks; `mithril-partial-sync.types.ts` and `MithrilPartialSyncStore.ts` were clean at HEAD and are clean again after `prettier --write` on just those two files
- **Deviations:** Formatting only, forced by the plan's own prettier gate: prettier 2.1.2 joins three of the plan's two-line snippets onto single lines — the factory signature in `mithril-partial-sync.types.ts` (`export const makeIdlePartialSyncStatus = (): MithrilPartialSyncStatusSnapshot => ({`), the store's `DEFAULT_STATUS` assignment, and the controller's `_partialSyncStatus` initializer. Semantics identical to the plan snippets; the plan's prettier row explicitly directs `prettier --write` on newly-failing touched files.
- **Notes:** Zero new or changed i18n messages (per plan; locale files and catalogs untouched). No task/finding IDs in source comments or test titles — new comments are the plan's plain rationale text. Nothing staged or committed; `.gitignore` and `.agent/skills/` untouched and left unstaged. N4 (plan section 2) not touched — it belongs to the next chunk. Known pre-existing `App.spec.tsx` failure (CAT-B `onRetry` wrapper) untouched by this chunk.

## Code Review — CAT-G chunk 1 round 1

- **Label:** Code Review
- **Scope:** CAT-G chunk 1 round 1 — plan section 1 (Steps 1.1–1.17) of `task-ux-703-plan-cat-g.md`: N2 dead epoch paths, N3 coordinator boolean getter, N5 shared idle-status factory, N6 static `runBinary` import, N8 no-op terminal filter removal, N9 story-constant hoist (N1 checklist-only no-op per DD-703-13; N7 out of scope, absorbed by CAT-B).
- **Timestamp:** 2026-07-03T20:14:41Z
- **Decision:** APPROVED
- **Checks performed:**
  - N2 (1.1–1.2): `extractCertifiedEpoch` path list trimmed to exactly `['beacon','epoch']` + `['cardano_db_beacon','epoch']`; replacement comment byte-identical to the plan; sibling `extractLatestCertifiedImmutableNumber` path list untouched; spec's bare-epoch preference test replaced with the plan's ignores-bare-epoch/non-beacon-spellings test verbatim.
  - N3 (1.3–1.7): coordinator's fabricated `MithrilPartialSyncAvailability` literal replaced by `isPartialSyncEnabled(): boolean`; unused type import removed; `PARTIAL_SYNC_DISABLED` seam untouched (CAT-E ownership respected); controller consumed only `isEnabled` (Step 1.5 escalation trigger did not fire) and still builds the full availability object itself — boundary 2/4 intact; controller spec's four mock renames applied with no `resolves.toEqual` payload changed; new coordinator boolean-getter test inserted exactly before the accessors test. Repo-wide grep: the only remaining `getPartialSyncAvailability` callers resolve to the CONTROLLER method (IPC channel + its spec), which keeps its name per the plan.
  - N5 (1.8–1.11): `makeIdlePartialSyncStatus` factory added after the snapshot type with the plan's rationale comment; controller (`DEFAULT_PARTIAL_SYNC_STATUS` deleted, `DEFAULT_BOOTSTRAP_STATUS` kept), service (`DEFAULT_STATUS` deleted), and store (local const kept, factory-sourced) all consume it via extended value imports. Grep confirms the only remaining `status: 'idle'` product-code sites are bootstrap-shaped (out of scope) plus the service's `_resetToIdleStatus` `_updateStatus(...)` call — an update-call argument with `logPath: undefined`, not a `DEFAULT_STATUS` reference; Step 1.10d only required replacing `DEFAULT_STATUS` references (grep: zero remain).
  - N6 (1.12): `await import('./mithrilCommandRunner')` gone; `runBinary` rides the existing static import; `_runBinary` stays async; service spec's module-level mock already provides `runBinary: jest.fn()` so the reported green run is plausible.
  - N8 + spec (1.13–1.14): overlay list is the plain concat of working + terminal arrays; new `mithril-partial-sync.types.spec.ts` is byte-identical to the plan snippet, and its 12-status list exactly matches the working (9) + terminal (3) arrays in the types module; no task/finding IDs in test titles.
  - N9 (1.15–1.17): `snapshotFilesTotal = 980` exported after `snapshotSize`; both story files import it (alphabetical placement per plan); repo-wide grep shows zero `SNAPSHOT_FILES_TOTAL` identifiers remain; all six usages renamed.
  - Boundaries/vocabulary/hygiene: zero user-facing copy and zero i18n changes (locale files untouched, per plan); no locked-boundary contact — renderer still receives backend-computed availability only, bootstrap flow untouched (types-module change verified a behavioral no-op; implementer's boundary-11 safety net 7 suites/103 tests green is plausible); no task/finding IDs in source comments; unstaged code diff covers exactly the 13 reported code files plus this log; `.gitignore` and `.agent/skills/` untouched and left unstaged.
  - Prettier independently re-run on all 13 files: `mithril-partial-sync.types.ts` and `MithrilPartialSyncStore.ts` clean; the five failing files (`chainStorageCoordinator.ts/.spec`, `MithrilController.ts/.spec`, `MithrilPartialSyncService.ts`) fail identically at their staged blobs, and a hunk-by-hunk drift diff shows every drift region lies OUTSIDE this chunk's edits (zero new drift). The reported single-line joins of three plan snippets (factory signature, store `DEFAULT_STATUS`, controller `_partialSyncStatus` initializer) are prettier-2.1.2-stable and semantically identical — accepted deviation, anticipated by the plan's prettier gate.
  - Implementer-reported runs (6 suites/186 tests targeted, 7 suites/103 tests boundary-11 net, `yarn compile` clean, lint exit 0) are plausible against the diff; the Verify phase re-runs everything.
- **Findings:** None blocking. Disclosed pre-existing prettier drift in the five staged-drift files is outside this chunk's hunks — accepted, consistent with the CAT-F precedent.
- **Action:** staged the chunk's 13 code/spec/story files (including the new types spec) plus this log.

## Implementation — CAT-G chunk 2 (N4 MithrilProgressView variant flag)

- **Label:** Implementation
- **Scope:** CAT-G chunk 2 — plan section 2 (Steps 2.1–2.13) of `task-ux-703-plan-cat-g.md`: N4 `MithrilProgressView` bootstrap/partial-sync `variant` flag replacing the 11 pre-formatted intl props the partial-sync overlay passed in (DD-703-14 mechanics), plus the stories/specs re-run gates.
- **Timestamp:** 2026-07-03T20:21:02Z
- **Outcome:** Completed. Every quoted anchor matched the working tree verbatim: the overlay call site carried exactly the 11 string props with exactly the message ids the plan quotes (the Step 2.11 escalation trigger did not fire — no swapped ids from CAT-C), all referenced descriptors exist in `MithrilBootstrap.messages.ts` with EN/JA locale entries, and no consumer other than the overlay passed any removed prop (grep across `MithrilBootstrap.tsx` and both story files: zero hits). No escalations triggered.
- **Files changed:**
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx` (Steps 2.1–2.10 — Props gains `variant?: 'bootstrap' | 'partial-sync'` and drops the 11 string props; destructuring defaults `variant = 'bootstrap'`; `isPartialSync` flag with the plan's replacement guard comment; `isCompletedTransition = status === 'completed' && isPartialSync`; `subtitleMessage`/`actionDisabledTooltip` computed before `return` (tooltip only for partial-sync + stopping-node); header/subtitle, node-stop, node-start frames resolve by variant; cancelling frame internalizes the partial-sync messages both variants already showed; completed-transition frame formats `partialSyncCompletedTransition` internally; action label is plain `messages.cancel`; the `actionDisabledTooltip ? <PopOver …` wrapper untouched)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` (Step 2.11 — the `<MithrilProgressView` element replaced byte-identically with the plan snippet: `variant="partial-sync"` added, all 11 pre-formatted intl props removed, every KEPT prop — including CAT-C's `hideAction` status list — preserved verbatim; `MithrilBootstrapMessages` import retained for the error view and recovery actions)
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx` (Step 2.12 — `renderComponent` gains the `variant` pass-through (param, type annotation, JSX prop); three new tests inserted byte-identically before 'does not show completion block for restore-complete state alone': partial-sync completed hand-off frame, bootstrap completed frame free of the hand-off, partial-sync node-stop copy; every pre-existing test untouched; plus one deviation below)
  - This log file (this entry)
- **Tests run:**
  - `yarn test:jest --testPathPattern "(MithrilProgressView|MithrilPartialSyncOverlay|MithrilBootstrap)"` — 7 suites, 106 tests green (103 pre-existing + 3 new; `MithrilPartialSyncOverlay.spec.tsx` green with ZERO edits as the plan requires; all pre-existing bootstrap tests unmodified and green — boundary 11 held)
  - `yarn compile` — clean (scss precompile succeeded natively; no scss touched by this chunk)
  - Targeted eslint on the three touched files — 0 errors (warnings are pre-existing classes: the overlay's unused `arg`/`error as any` predate this chunk; the two `no-explicit-any` warnings in the new spec stub exactly mirror the identical pre-existing stub in `MithrilPartialSyncOverlay.spec.tsx`)
  - `prettier --check` on the three touched files — all clean, no `--write` needed
- **Deviations:** One test-environment addition the plan did not anticipate: the new partial-sync stopping-node test is the first VIEW-spec test to render the disabled-Cancel tooltip path, and react-polymorph's `PopOver` needs a skin/theme context the spec does not provide (it threw `Element type is invalid … Check the render method of PopOver`). Added to `MithrilProgressView.spec.tsx` the byte-identical `jest.mock('react-polymorph/lib/components/PopOver', …)` stub (with the same rationale comment) that `MithrilPartialSyncOverlay.spec.tsx` already carries for exactly this path — test-only, no product code affected, no plan assertion altered, no pre-existing test touched.
- **Notes:** Zero new or changed i18n messages (per plan; locale files, catalogs, and `MithrilBootstrap.messages.ts` untouched). Bootstrap-variant output verified byte-identical by construction: with the default `variant`, every frame resolves the exact descriptor the old `x || intl.formatMessage(messages.y)` fallbacks produced, `isCompletedTransition` stays false, and the tooltip stays `undefined` (no PopOver wrapper). No task/finding IDs in source comments or test titles. Nothing staged or committed; unstaged diff covers exactly the three chunk files plus this log; `.gitignore` and `.agent/skills/` untouched and left unstaged. Story files need no edits (neither passes any removed prop — re-verified by grep). Known pre-existing `App.spec.tsx` failure (CAT-B `onRetry` wrapper) untouched by this chunk.

## Code Review — CAT-G chunk 2 round 1

- **Label:** Code Review
- **Scope:** CAT-G chunk 2 round 1 — plan section 2 (Steps 2.1–2.13) of `task-ux-703-plan-cat-g.md`: N4 `MithrilProgressView` bootstrap/partial-sync `variant` flag (DD-703-14 mechanics) with locked boundary 11 (bootstrap completed frame byte-identical) plus the stories/specs re-run gates.
- **Timestamp:** 2026-07-03T20:23:57Z
- **Decision:** APPROVED
- **Checks performed:**
  - Steps 2.1–2.10 (`MithrilProgressView.tsx`): diff byte-matches every plan snippet — Props block gains `variant?: 'bootstrap' | 'partial-sync'` and drops exactly the 11 string props; destructuring defaults `variant = 'bootstrap'`; `isPartialSync` flag plus the plan's replacement guard comment; `isCompletedTransition = status === 'completed' && isPartialSync`; `subtitleMessage`/`actionDisabledTooltip` computed before `return` (tooltip only for partial-sync + stopping-node); header, node-stop, node-start frames resolve by variant; cancelling frame internalizes the partial-sync descriptors both variants already showed; completed-transition frame formats `partialSyncCompletedTransition`; action label is plain `messages.cancel`; the `actionDisabledTooltip ? <PopOver …` wrapper untouched and now consumes the locally computed variable.
  - Boundary 11 by construction: for the default bootstrap variant every rendered string resolves the exact descriptor the old `x || intl.formatMessage(messages.y)` fallbacks produced (bootstrap callers never passed any removed prop), `isCompletedTransition` stays false, and the tooltip stays `undefined` (no PopOver wrapper) — bootstrap output including the completed frame byte-identical. All pre-existing view-spec tests are unmodified in the diff; implementer's 7-suite/106-test green run (103 pre-existing + 3 new, overlay spec green with zero edits) is plausible against the diff.
  - Step 2.11 (`MithrilPartialSyncOverlay.tsx`, unstaged hunks only — staged hunks belong to CAT-C): `variant="partial-sync"` added; all 11 pre-formatted intl props removed; every KEPT prop (including the `hideAction` status list) preserved verbatim as unchanged context lines; `MithrilBootstrapMessages` import retained with 7 remaining usages (error view / recovery actions).
  - Step 2.12 (`MithrilProgressView.spec.tsx`): `renderComponent` variant pass-through (param, type annotation, JSX prop) and the three new tests inserted byte-identically before 'does not show completion block for restore-complete state alone'; the pre-existing status union already contains 'stopping-node'; every existing test untouched.
  - Deviation verified: the added `jest.mock('react-polymorph/lib/components/PopOver', …)` stub is byte-identical (including the rationale comment) to the one `MithrilPartialSyncOverlay.spec.tsx` already carries for the same tooltip path; test-only, and no pre-existing view-spec test reaches the PopOver branch (bootstrap tooltip is always `undefined`), so it cannot mask a bootstrap regression. Accepted.
  - i18n: zero new or changed messages; all descriptors referenced by the variant logic exist in `MithrilBootstrap.messages.ts`; spot-checked `en-US.json` — `loading.mithrilPartialSync.title` ("Mithril Sync"), `completed.subtitle`, `completed.transition`, `progress.nodeStoppingDetail` all present and matching the new tests' assertions. Vocabulary clean: user-facing copy says "Mithril Sync", no percentages, no "immutable"; "partial-sync" appears only as internal identifier/variant value and in test titles naming that identifier, per plan snippets.
  - Hygiene: no task/finding IDs in source comments or test titles; unstaged diff covers exactly the three chunk files plus this log; `.gitignore` and `.agent/skills/` untouched by this chunk and left unstaged; no story edits (plan confirms neither story file passes any removed prop); nothing committed; no GitHub access.
  - Implementer-reported runs (`yarn compile` clean, targeted eslint 0 errors with only pre-existing warning classes, `prettier --check` clean on all three files) are plausible against the diff; the Verify phase re-runs everything.
- **Findings:** None blocking. The PopOver-stub deviation is disclosed, minimal, and mirrors existing precedent in the sibling overlay spec.
- **Action:** staged the chunk's three code/spec files plus this log.

## Implementation verify-fix — Gate 4 (prettier) and Gate 5 (jest mithril)

- **Label:** Implementation verify-fix
- **Scope:** Remediate the two verification-gate failures: the task-introduced prettier violation in `MithrilPartialSyncService.spec.ts` and the `App.spec.tsx` `onRetry` identity assertion left stale by the CAT-B retry wrapper.
- **Timestamp:** 2026-07-03T20:35:58Z
- **Files changed:**
  - `source/main/mithril/MithrilPartialSyncService.spec.ts` — reflowed the `require('./mithrilPartialSyncMarker').readMithrilPartialSyncMarker as jest.Mock` declaration inside the task-added 'staging root resolution from the durable marker' describe to prettier 2.1.2's fixpoint (`const … = require(…)` on one line, member access continued on the next). The identical pre-existing declaration (HEAD line 1845, now 2082) left untouched.
  - `source/renderer/app/App.spec.tsx` — 'mounts the partial sync overlay and forwards all recovery callbacks': `onRetry` assertion changed from store-method identity to `expect.any(Function)` because App now passes an inline wrapper that calls `startPartialSync().catch(…)`; the click-through assertion (`startPartialSync` called once) still proves forwarding. `startPartialSync` mock now `mockResolvedValue(undefined)` so the wrapper's `.catch` has a promise to attach to.
  - This log file (this entry)
- **Tests run:**
  - `yarn test:jest --testPathPattern mithril` — 63 suites, 759/759 green (previously 1 failed); all 5 mithril-bootstrap suites green, boundary 11 held.
  - Gate 4 confirmation: `prettier` output of the working-tree `MithrilPartialSyncService.spec.ts` diffed against the file — the only remaining drift regions (94, 2082) are byte-identical to HEAD's pre-existing drift (93, 1845), verified with the same HEAD-blob stdin-filepath method used to classify the other 14 files; `prettier --check` clean on `App.spec.tsx` and `App.tsx`.
- **Deviations:** None. No product code touched — both fixes are test/format-only; the retry wrapper behavior in `App.tsx` (CAT-B) is unchanged.
- **Notes:** The prettier fix is a stable fixpoint (not the known `toHaveBeenCalledWith` oscillation). No i18n changes. Staged exactly the two spec files; `.gitignore` and `.agent/skills/` untouched; nothing committed; no GitHub access.

## Scribe — task-ux-703 documentation closeout

- **Label:** Scribe
- **Scope:** Final documentation closeout after all CAT-A–G chunks were implemented, reviewed (all APPROVED), and the 6-gate verification ran green (lint 0 errors; tsc clean; jest mithril 63/63 suites, 759/759 tests incl. bootstrap boundary-11 suites; prettier failures 100% pre-existing drift; i18n catalogs complete EN+JA, `yarn i18n:manage` exit 0; scss d.ts current).
- **Timestamp:** 2026-07-03T20:51:38Z
- **Files changed:**
  - `task-ux-703.md` — Build/Review statuses set to `completed`; Outcome section added (per-CAT summary incl. the T23 mid-build amendment; references the per-section plan docs `task-ux-703-plan-cat-a.md` … `-cat-g.md`)
  - `../../mithril-partial-sync-ux-refinement-tasks.json` — task-ux-703 `status: completed`, `completedAt: 2026-07-03`
  - `task-ux-703-research.md` — appended implementation findings (breadth of pre-existing prettier 2.1.2 drift + the `git show | prettier --stdin-filepath` classification technique; T23/CAT-E cross-plan conflict and Amendment resolution; preflight `layoutResult.managedChainPath` reuse; JA strings flagged for native review)
  - `../../mithril-partial-sync-smoke-test-cheat-sheet.md` — DD-703-7 gaps filled: Diagnostics section documented as always visible when enabled (near-tip informational + probe-failed "availability unknown" states, CTA enabled), post-success row copy corrected, completed-overlay finalize-failure → error-view-with-retry path added (disk-preflight numbers were already updated by CAT-D)
  - `../../mithril-partial-sync-ux-refinement-prd.md` — pointer lines added under D7 (DD-703-5 delta-based preflight) and D9 (ADR D-702a-1 + DD-703-4 finalize-failure path)
  - `task-ux-703-pr-comment-checklist.md` — T23 and T22 responses updated to the amended helper semantics (known codes resolve mapped intl copy via the code-keyed map; single shared intl fallback otherwise; raw prose/codes never render)
  - This log file (this entry)
- **Outcome:** task-ux-703 closed — all 32 threads and 9 nits remediated or checklist-answered, verification green, documentation finalized; doc files staged, nothing committed, no GitHub writes.
