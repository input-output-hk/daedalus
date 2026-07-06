# task-ux-704 — Implementation review log

Per-CAT commit + review record for the task-ux-704 remediation wave. Append-only; do not
rewrite prior entries.

## CAT-A

- **Timestamp:** 2026-07-06T06:38:54Z
- **Commit:** `519b69ff5fc8d5737c82dc704f87f93f6405f7e6` — `refactor(mithril): task-ux-704 CAT-A delete dead ipc facade exports and dead main-process plumbing`
- **Files in commit (13):**
  - `source/main/ipc/mithrilBootstrapChannel.spec.ts`
  - `source/main/ipc/mithrilBootstrapChannel.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.ts`
  - `source/main/mithril/MithrilBootstrapService.ts`
  - `source/main/mithril/MithrilController.spec.ts`
  - `source/main/mithril/MithrilController.ts`
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/mithril/MithrilPartialSyncService.ts`
  - `source/main/mithril/MithrilStartupGate.ts`
  - `source/main/mithril/mithrilCommandRunner.ts`
  - `source/main/utils/handleDiskSpace.spec.ts`
  - `source/main/utils/handleDiskSpace.ts`

### Implementation summary

- Deleted the four dead partial-sync channel shims (A-1) and the eight dead bootstrap
  channel wrappers (A-2), keeping the S1 keep-list (`emitMithrilPartialSyncStatus`,
  `getMithrilPartialSyncStatus`) and the two live bootstrap wrappers intact.
- Retargeted `isMithrilDecisionCancelledError` to its canonical `mithrilDecision` source
  and dropped the two dead re-export lines in the channel and controller (A-3).
- Removed production-empty status-listener plumbing on `MithrilController` (A-4), the
  write-only `_logStream` on `MithrilPartialSyncService` plus the optional `MithrilBootstrapService`
  rider and the now-orphaned `onLogStream` runner plumbing (A-5), the dead `stdinInput`
  runner option (A-6), the zero-caller `getStartupGateState`/gate `state` getter (A-7),
  and the spec-only public `listSnapshots`/`showSnapshot` wrappers (A-8).
- Net diff: 12 insertions / 277 deletions — pure deletions plus one import retarget and
  comment fixups; no IPC channel names, emitted status shapes, or user-visible copy changed.

### Review outcome

Approved by the adversarial review panel. No blockers required fixing at commit time.

### Optional steps taken / skipped

- **A-5 optional rider (`MithrilBootstrapService.ts` + runner `onLogStream` teardown):**
  TAKEN — file is present in the commit and the runner `onLogStream` plumbing was removed
  since no registrar remained repo-wide.

### Sanctioned behavior delta

None. Per the plan this CAT is pure deletion plus one canonical-import retarget; a revert
restores HEAD behavior exactly. The one planned test removal
(`'serves snapshot list and show reads from the shared metadata pipeline'`) is a dead-wrapper
spec, not a behavior change.

## CAT-B

- **Timestamp:** 2026-07-06T07:07:57Z
- **Commit:** `3ce34ad67b49f7de4e84b4dab5d0de1d624b7f71` — `refactor(mithril): task-ux-704 CAT-B break ipc import cycle and consolidate main-process structure`
- **Files in commit (13):**
  - `source/main/mithril/mithrilPartialSyncNodeStartup.ts`
  - `source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts`
  - `source/main/utils/handleDiskSpace.ts`
  - `source/main/utils/handleDiskSpace.spec.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.ts`
  - `source/main/ipc/mithrilPartialSyncChannel.spec.ts`
  - `source/main/mithril/mithrilErrors.ts`
  - `source/main/mithril/mithrilPartialSyncPreflight.ts`
  - `source/main/mithril/mithrilPartialSyncStaging.ts`
  - `source/main/mithril/MithrilPartialSyncService.ts`
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`
  - `source/main/mithril/MithrilBootstrapService.ts`
  - `source/main/mithril/MithrilStartupGate.ts`

### Implementation summary

- Broke the mithril→ipc→mithril import cycle (B-1) by injecting `emitPartialSyncStatus` /
  `getPartialSyncStatus` into `MithrilPartialSyncNodeStartup` via `NodeStartupDependencies`,
  wiring them at the sole `handleDiskSpace.ts` construction site through
  `getMithrilController().broadcastPartialSyncStatus` / `.getPartialSyncStatus`, then
  deleting the two now-orphaned channel exports (S1 endgame) and the inert spec mocks.
- Moved `MithrilPartialSyncStageError` + `createPartialSyncStageError` into `mithrilErrors.ts`
  (B-2), deleted the `PartialSyncStageErrorFactory` param threaded through the 5 preflight +
  3 staging exports, and removed the six identical factory lambdas from the service (`code`
  kept as plain `string` — CAT-D owns the S2 union tightening).
- Dropped the deadweight `workDir` param from five `MithrilBootstrapService` members and the
  duplicate `_currentProcess = null` in `cancel()` (B-3, both fields + `setWorkDir` kept);
  deduped the startup-gate declines into one parameterized `_handleDecline` + a
  `_handleTerminalStatusDecline` prologue helper and dropped the dead `value` param on
  `_markHadNotEnoughSpaceLeft` (B-4).
- Small legibility (B-5): `_getFallbackErrorStage` is now a membership check; the duplicate
  cancel-entry log line was removed. Standardized `MithrilBootstrapService` log prefixes to
  `MithrilBootstrapService:` and collapsed the finalize narration triplet (B-6, pure log
  strings).
- Net diff: 213 insertions / 317 deletions. Every edit behavior-preserving — same
  `broadcastPartialSyncStatus` emission path/snapshot seam, byte-identical error
  name/message/stage/code, byte-identical wipe messages, no IPC channel names or emitted
  status shapes or user-visible copy changed.

### Review outcome

Approved by the adversarial review panel. No blockers required fixing at commit time.

### Optional steps taken / skipped

- None deferred within CAT-B scope. Explicitly out-of-scope items (A1 file split, A3 runner
  injection, A2 gate enum/`_transition` removal, `_workDir`/`_activeWorkDir` collapse, S2
  union typing, CAT-C `chainEmpty` branch) were left untouched per the plan's Out-of-scope
  section.

### Sanctioned behavior delta

Two accepted transient-window tightenings, both confined to an already-cancelled,
mid-teardown run whose output is discarded (documented as equivalence notes in the plan):
(1) dropping the `workDir` params moves default evaluation from method entry to use time;
(2) removing the in-branch `_currentProcess = null` leaves the killed child in the slot
during cancel's cleanup await, so a concurrent `startBootstrap` throws
`'Mithril bootstrap already in progress'` until the child's `close` event clears the slot.
No user-visible or renderer-observable behavior changes.

## CAT-C

- **Timestamp:** 2026-07-06T07:34:48Z
- **Commit:** `0b6768b0eb1495600b3991089a49120d9757f825` — `refactor(mithril): task-ux-704 CAT-C de-fork chain path resolution and prune dead chain-storage code`
- **Files in commit (12):**
  - `source/main/utils/chainStorageManager.ts`
  - `source/main/utils/chainStorageManagerConfig.ts`
  - `source/main/utils/chainStorageManagerShared.ts`
  - `source/main/utils/chainStorageManagerLayout.ts`
  - `source/main/utils/chainStorageCoordinator.ts`
  - `source/main/utils/chainStoragePathResolver.ts`
  - `source/main/utils/handleDiskSpace.ts`
  - `source/main/mithril/MithrilPartialSyncService.ts`
  - `source/main/utils/chainStorageManager.spec.ts`
  - `source/main/utils/chainStoragePathResolver.spec.ts`
  - `source/main/utils/handleDiskSpace.spec.ts`
  - `source/main/mithril/MithrilPartialSyncService.spec.ts`

### Implementation summary

- De-forked path resolution (C-1): extracted `deriveCustomPathFromChainState` into
  `chainStorageManagerShared.ts`, had `getConfig` reuse it, and pointed `getManagedChainPath`
  (via new `_getActiveCustomPath`) and `resolveDiskSpaceCheckPath` at the pure derivation so
  hot callers no longer fork `checkDiskSpace` just to learn a path; four now-stale fork
  rationale comments in `MithrilPartialSyncService.ts`/`.spec.ts` corrected.
- Deleted the inert `chainEmpty` pre-check branch in `handleDiskSpace.ts` (C-2), collapsing to
  the single `handleStoppedNodeStartup` call the gate already gates internally.
- Pruned dead public API (C-3): removed `migrateData`, `getResolvedManagedChainPath`,
  `getManagedParentPath`, both `resolveChainStoragePath` exports, and
  `getMithrilPartialSyncDisabledError`; collapsed the resolver to one correctly-documented
  export (`resolveMithrilWorkDir` stays live) and moved its two win32-junction tests over.
- Deduped the identical `_withMutationLock` bodies into shared `runSerializedMutation` (C-4,
  FIFO/swallow/warn-rethrow/synchronous-swap preserved), plus the C-5 trivia batch
  (unreachable throw, twin switch-arm merge, `validation` shadow rename, feature-flag
  delegation, `buildLayout` helper, seven unused `_ctx` params).
- Net diff: 203 insertions / 401 deletions. Every edit behavior-preserving; no IPC
  channel names/payloads, log strings (except the one new `_getActiveCustomPath` warn), or
  user-visible copy changed.

### Review outcome

Approved by the adversarial review panel. No blockers required fixing at commit time.

### Optional steps taken / skipped

- Out-of-scope items (C1 manager re-decomposition, C2 coordinator re-architecture, C7 refuted,
  C8/C9, the `handleDiskSpace` single-flight queue, de-forking `setDirectory`/`prepareForLocationChange`,
  and the observation-only `ChainStorageCoordinator.resolveDiskSpaceCheckPath`) were left
  untouched per the plan's Out-of-scope section.

### Sanctioned behavior delta

One accepted strict correctness improvement (C-1 equivalence case 5): on the probe-failure
error path where a healthy symlink existed, the old code fell into `getConfig`'s catch and
returned `customPath: null` — silently redirecting emptiness/disk checks to the default
location. The de-forked reader never runs the probe, so it returns the correct custom path.
Plus one new `logger.warn` line for `_getActiveCustomPath` derivation failure. No
user-visible or renderer-observable behavior changes.

## CAT-D

- **Timestamp:** 2026-07-06T07:51:28Z
- **Commit:** `13e72cf71c37f6f472a314261d85066676295f2d` — `refactor(mithril): task-ux-704 CAT-D type-contract tightening`
- **Files in commit (11):**
  - `source/common/types/mithril-partial-sync.types.ts`
  - `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts`
  - `source/main/mithril/mithrilErrors.ts`
  - `source/main/mithril/mithrilPartialSyncPreflight.ts`
  - `source/main/mithril/MithrilPartialSyncService.ts`
  - `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`
  - `source/renderer/app/stores/MithrilPartialSyncStore.ts`
  - `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`
  - `source/renderer/app/components/status/MithrilPartialSyncSection.tsx`
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`

### Implementation summary

Compile-time-only type-contract tightening across the partial-sync surface. D-1 grew the
`MithrilPartialSyncErrorCode` union 13 → 18 (five real preflight/staging wire codes, all
mapped to the generic `FAILED` copy pair — provably copy-neutral since they emit at stage
`preparing`, absent from `COPY_BY_STAGE`), typed `MithrilPartialSyncError.code` as the union,
and union-typed the four declaration sites (class field + ctor param + `createPartialSyncStageError`
in `mithrilErrors.ts`, `statRequiredPath`'s `code` param, and the service's `_createStageError`),
dropping the `:91` cast while keeping the code-as-message `resolvePartialSyncErrorCopyByCode`
cast intact. D-2 removed the never-crossed `| null` arm from `certifiedEpoch` (types/service/
store) and rewrote its stale comment. D-3 typed `progressItems` as `MithrilProgressItem[]`.
D-4 collapsed three double-cast pairs in `MithrilStepIndicator.tsx` into one local
`isAnyRestoreCompleteStatus` helper. D-5 tightened the section's `onStartMithrilPartialSync`
prop to `() => Promise<void>` and typed `shouldCloseDiagnosticsForPartialSyncOverlay`'s params.
D-6 deleted the prod-dead `isActive` computed and `isMithrilPartialSyncActiveStatus` guard
plus its one spec assertion. Four overlay-story fixtures dropped their out-of-union `code`
fields (compile-forced) with a corrected fixture comment.

### Review outcome

Approved by the adversarial review panel. No blockers required fixing at commit time.

### Optional steps taken / skipped

- D-6 (the optional prod-dead `isActive`/`isMithrilPartialSyncActiveStatus` micro-deletion,
  E3-sanctioned as droppable) was **taken**, congruent with the wave's dead-code standard.
- All Out-of-scope items (D2 push model, D8 dead `logPath`, the 9-observables snapshot
  refactor, D11 layering move, `resolvePartialSyncErrorCopyByCode` typing, the
  `DaedalusDiagnostics` handler-block Props, and all comment-inventory work owned by CAT-H)
  were left untouched per the plan.

### Sanctioned behavior delta

One accepted story-visible delta (Step 1.7): in the four affected overlay fixtures
(`cancelledError`, `restartAllowedError`, `wipeOnlyError`, `finalizingError`) the error-code
line inside the collapsed technical-details section disappears, since those fixtures carried
story-invented codes outside the union. All resolved titles/hints/messages/log-path links are
unchanged, and no production runtime, IPC, or user-facing copy behavior changes.

## CAT-E

- **Timestamp:** 2026-07-06T08:11:25Z
- **Commit:** `351be19e9fa41c3e87530783decc74855878537b` — `refactor(mithril): task-ux-704 CAT-E renderer cleanups`
- **Files in commit (13):**
  - `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`
  - `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
  - `source/renderer/app/components/status/MithrilPartialSyncSection.tsx`
  - `source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx`
  - `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`
  - `storybook/stories/nodes/status/Diagnostics.stories.tsx`
  - `source/renderer/app/stores/MithrilBootstrapStore.ts`
  - `source/renderer/app/stores/MithrilBootstrapStore.spec.ts`
  - `source/renderer/app/components/chain-storage/chainStorageUtils.ts`
  - `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.tsx`
  - `source/renderer/app/components/status/DiagnosticsTimeStatusRow.tsx`
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx`
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`

### Implementation summary

- E-1 deleted the dead `showMithrilPartialSyncConfirmationOnOpen` deep-link pipeline end-to-end
  (dialog hardwired `false`, `DaedalusDiagnostics` Props/destructure/pass-down, the section's
  `showConfirmationOnOpen` prop and its `componentDidMount` auto-open branch); dropped the three
  deep-link specs and reworked the epochs-threading spec onto the CTA-click seam; reworked the
  storybook "Partial Sync Confirmation" story to drive the section's real confirmation seam via a
  mount-time CTA click and removed the falsified fixture comment.
- E-2 replaced the forged `chainSubdirectoryStatus: 'will-create'` validation in
  `returnToStorageLocation` with a real revalidation of the previous custom path, degrading to the
  provisional draft only on validate failure; added the fallback spec and pinned the happy path.
- E-3 dropped the unreachable `'existing-directory'` help-text branch and collapsed the twin help
  `<p>` blocks to one, and deleted the dead hardcoded-English `message` field. E-4 de-duplicated
  the four diagnostics descriptors by exporting the `defineMessages` const and consuming it in the
  row. E-5 removed the noise `as any` on the already-typed overlay `error` prop and the spec-only
  guard re-export (repointing the spec at the common types module).
- Net diff: 117 insertions / 109 deletions across 13 files.

### Review outcome

Approved by the adversarial review panel. No blockers required fixing at commit time.

### Optional steps taken / skipped

- The PRD D13 residual-note update (Step 1.7) is a working-tree edit to
  `mithril-partial-sync-ux-refinement-prd.md` but is intentionally NOT in this commit: the
  committer scope excludes `.agent/` docs. All Out-of-scope items (E1 finalize relocation, E4
  shared start-handler util, story fixture typing / `as any` removals owned by CAT-F, deleting the
  orphaned `subdirectoryWarningExists` descriptor) were left untouched per the plan.

### Sanctioned behavior delta

One wave-sanctioned D12 defect fix (E-2), wrong-to-right only: when returning to a previous custom
path that already contains a chain subdirectory, the picker's helper copy previously claimed a
subdirectory "will be created" (hardcoded `'will-create'`); it now reflects the real validation
(`'existing-directory'` selects the data-found notice instead). No message text changes — only
which existing message is selected. A validate-channel failure degrades to exactly today's
provisional draft. No other user-visible, IPC, or renderer-observable behavior changes.

## CAT-F

- **Timestamp:** 2026-07-06T08:23:18Z
- **Commit:** `a62da7b63f8208d7730f3f5b61301deeef000912` — `refactor(mithril): task-ux-704 CAT-F harden partial sync storybook stories and fixtures`
- **Files in commit (6):**
  - `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`
  - `source/renderer/app/components/status/DaedalusDiagnostics.tsx`
  - `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`
  - `storybook/stories/loading/_support/mithrilFixtures.ts`
  - `storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx`
  - `storybook/stories/nodes/status/Diagnostics.stories.tsx`

### Implementation summary

- F-1 exported `DOWNLOAD_PROGRESS_ANCHOR_ID` from `MithrilStepIndicator.tsx` (visibility only)
  and replaced the six hand-copied `id: 'step-3'` literals in the overlay story (1) and
  `mithrilFixtures.ts` (5) with the imported constant, so a wrong anchor id can no longer
  silently drop the combined download-progress bar; the hand-copy-hazard comment was rewritten.
- F-2 rewired the dialogue story's confirm-view seam from a copy-string DOM `textContent`
  match to a locale/copy-independent `.scss` `.primaryAction` class selector that throws loudly
  when the button disappears (no production change).
- F-3 typed `Diagnostics.stories.tsx` `baseProps` via `ComponentProps<typeof DaedalusDiagnostics>`,
  filled the missing required `isMithrilPartialSyncProbeFailed: false`, corrected the two
  `coreInfo` PID fields to strings, removed the `as any` / `: any` casts, and typed the
  confirmation factory; fixed the `onRestartNode` Props inaccuracy at source in
  `DaedalusDiagnostics.tsx` to `{ trigger }` and deleted the now-stale `ts-migrate(2339)`
  suppression above the `.trigger()` call.
- F-4 switched the overlay story's three numeric fallbacks (`filesDownloaded`/`filesTotal`/
  `elapsedSeconds`) from `||` to `??` so zero-progress states are expressible; boolean `|| false`
  fallbacks left untouched.
- S3 gate confirmed satisfied (CAT-E landed; `showMithrilPartialSyncConfirmationOnOpen` gone).
  Net diff: 43 insertions / 34 deletions across 6 files.

### Review outcome

Approved by the adversarial review panel. No blockers required fixing at commit time.

### Optional steps taken / skipped

- The optional author-run interactive `yarn storybook` inspection was skipped (compile/lint/jest
  gates cover the typing enforcement). All Out-of-scope items (F5 `satisfies` fixture typing, F9a
  harness hook extraction, F10 preset builder / duplicate presets, E9i id-namespace rename, the
  refuted F7 typed-field variant, spec/e2e literal hygiene) were left untouched per the plan.

### Sanctioned behavior delta

None. Every edit is type/visibility-only (F-1 `export` keyword, F-3 Props type line + stale
suppression removal) or story-internal (F-2 seam, F-4 fallback operator). No IPC names, emitted
status shapes, or user-visible copy changed; all existing stories render byte-identically (no
story passes `0` or relies on the removed cast paths).

## CAT-G

- **Timestamp:** 2026-07-06T08:33:21Z
- **Commit:** `2dbb4b1a89b0658b65e018ee3d591de373f72b08` — `chore(mithril): task-ux-704 CAT-G remove agent tooling debris from repo root`
- **Files in commit (4):**
  - `ralph.sh` — deleted
  - `opencode.jsonc` — deleted
  - `review_output.json` — deleted
  - `.gitignore` — three root-anchored ignore entries appended

### Implementation summary

- G-1 removed the three branch-introduced agent-tooling artifacts from the repo root
  (`ralph.sh` 262-line opencode agent-loop driver, `opencode.jsonc` 60-line tool config,
  `review_output.json` 35-line one-off review artifact) via `git rm` and added root-anchored
  `.gitignore` entries (`/ralph.sh`, `/opencode.jsonc`, `/review_output.json`) so regenerated
  local copies never silently re-track.
- Step 1.1 reference gate re-run (`git grep -nE "ralph\.sh|opencode|review_output" --` excluding
  the three files): all hits fall under `.agent/` documentation (historical narrative) — zero
  references in `source/`, `tests/`, `storybook/`, `nix/`, `package.json`, or CI. Gate passes; no
  E1 escalation.
- `git check-ignore -v` attributes all three names to the new `.gitignore:143-145` root-anchored
  rules. `git show --stat HEAD` lists exactly the 4 intended paths.
- Net diff: 5 insertions / 357 deletions. Pure repo-hygiene deletion — no source, spec, i18n, or
  runtime surface touched.

### Review outcome

Approved by the adversarial review panel. No blockers required fixing at commit time.

### Optional steps taken / skipped

- The optional `touch ralph.sh` / `git status` clean-confirmation check was skipped;
  `git check-ignore -v` already deterministically attributed all three names to the new rules.
- Local-drift procedure (Step 1.3 steps 2/5): no `.devcontainer` line existed in the working-tree
  `.gitignore` at commit time (the `.devcontainer/` dir is separately untracked, not a `.gitignore`
  edit), so `git diff .gitignore` was already clean and the delete/re-insert steps were correctly
  skipped. Out-of-scope items (the `.agent/` ralph/opencode narrative mentions, upstream ride-along
  hygiene) were left untouched per the plan.

### Sanctioned behavior delta

None. Pure deletion of untracked-worthy tooling debris plus ignore entries; a `git revert`
restores all three files and removes the ignore rules in one step. No product, IPC, or
user-visible behavior changes.

## CAT-H

- **Timestamp:** 2026-07-06T09:12:52Z
- **Commit:** 73112b753 — `refactor(mithril): task-ux-704 CAT-H prune review-narration comments across partial sync`
- **Files in commit (42):** source/main/cardano/CardanoNode.ts, source/main/index.ts, source/main/ipc/mithrilBootstrapChannel.ts, source/main/mithril/MithrilBootstrapService.ts, source/main/mithril/MithrilController.spec.ts, source/main/mithril/MithrilController.ts, source/main/mithril/MithrilPartialSyncService.spec.ts, source/main/mithril/MithrilPartialSyncService.ts, source/main/mithril/killProcessTree.ts, source/main/mithril/mithrilCommandRunner.spec.ts, source/main/mithril/mithrilCommandRunner.ts, source/main/mithril/mithrilPartialSyncMarker.spec.ts, source/main/mithril/mithrilPartialSyncNodeStartup.spec.ts, source/main/mithril/mithrilPartialSyncNodeStartup.ts, source/main/mithril/mithrilSnapshotMetadata.ts, source/main/utils/chainStorageCoordinator.spec.ts, source/main/utils/chainStorageValidation.spec.ts, source/main/utils/handleDiskSpace.spec.ts, source/renderer/app/App.tsx, source/renderer/app/components/chain-storage/ChainStorageLocationPicker.spec.tsx, source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.spec.tsx, source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx, source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx, source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx, source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.tsx, source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx, source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx, source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx, source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx, source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx, source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx, source/renderer/app/components/status/MithrilSyncProcessSummary.messages.ts, source/renderer/app/containers/loading/MithrilProactivePromptContainer.spec.tsx, source/renderer/app/containers/loading/MithrilProactivePromptContainer.tsx, source/renderer/app/containers/loading/SyncingConnectingPage.tsx, source/renderer/app/stores/MithrilPartialSyncStore.spec.ts, source/renderer/app/stores/MithrilPartialSyncStore.ts, source/renderer/app/stores/NetworkStatusStore.ts, source/renderer/app/utils/mithrilBehindness.ts, source/renderer/app/utils/mithrilErrorMessage.ts, storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx, storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx

### Implementation summary

Final commit of the task-ux-704 wave (seam S4): the comment-cleanup inventory, ridden last so
its anchors sit on post-CAT-A–G code. Stripped reviewer-directed narration, checkpoint labels
(CP-A..CP-D), ALL-CAPS emphasis, and code-restating prose across 42 source/spec/story files
while preserving the load-bearing invariants (slot-clobber safety, cancel-never-reaches-cutover,
detached-child sync-shutdown reap, disk fail-closed floor, MobX strict-mode/runInAction, Electron
41 SIGABRT, epochs-only vocabulary). Net 179 insertions / 757 deletions — comment-only, except the
one authorized test rename dropping a `CP-D` label from an `it()` title in
MithrilPartialSyncService.spec.ts. Entries mooted by earlier CATs were skipped, not re-applied.

### Review outcome

Approved by the adversarial review panel. No blockers required fixing at commit time.

### Optional steps skipped

None beyond the inventory's own mooted/KEEP-by-omission entries (comments already matching the
convention or handled inside CATs A–G were left as those CATs left them).

### Sanctioned behavior delta

None. Comment-only cleanup plus one authorized test-title rename; no product, IPC, or
user-visible behavior changes. `git revert` of this single commit backs it out with no code-path
impact.
