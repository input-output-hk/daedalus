# task-ux-702c — Implementation Review (manual-testing cleanup wave)

> Implementation + code-review log for the CAT-A...CAT-F slices of task-ux-702c.
> Plan: `task-ux-702c.md`. Decisions: `task-ux-702c-decisions.md` (D-702c-0..13).

---

## Implementation

- 2026-07-01 — CAT-A implemented on the shared working tree. Added the shared `cancelling` partial-sync status, split backend cancel into `cancel` plus `finalizeCancel`/`forceKill`/`abandonCancel`, gated retry-enabled terminal emission on the coordinator-owned run-promise join, routed `cancelling` through the partial-sync progress overlay with cleanup copy and no actions/timer/waterfall, added the swallowed-start warning in the renderer store, updated CAT-A backend/store/overlay/story coverage, and reconciled the PRD status contract by replacing the phantom `confirming` entry with `cancelling`.
- 2026-07-01 — CAT-B implemented on the shared working tree. Updated the shared Mithril bootstrap `.backdrop` rule to import and reuse the existing `overlay-backrop` mixin, kept the existing Mithril tint token, and set `opacity: 1` so the overlay uses the app-wide frosted-glass blur without introducing new classes, colors, or theme variables. Narrow executable checks run for this slice: `./node_modules/.bin/sass source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss >/tmp/task-ux-702c-cat-b.css` and `./node_modules/.bin/stylelint source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss`, both passing. Operator on-device proof that the live syncing screen no longer bleeds through the overlay remains a separate pending manual-validation item under U2.
- 2026-07-01 — CAT-C validation-only pass: **no code change**. Verified the Diagnostics gate at
	`DaedalusDiagnostics.tsx:724-727` still uses the AND-gate
	(`isMithrilPartialSyncEnabled && isMithrilPartialSyncSignificantlyBehind`), with `behindByEpochs` remaining
	display-only in `MithrilPartialSyncSection.tsx`. Verified proactive prompt suppression at
	`MithrilProactivePromptContainer.tsx:67-74` remains coupled to `isSignificantlyBehind`, and the unchanged
	regression test `DaedalusDiagnostics.spec.tsx:119-139` still proves the OR-gate was never introduced. Boundary #4
	documentation in `mithrilBehindness.ts` remains intact.
- 2026-07-01 — CAT-D + CAT-E implemented on the shared working tree as one coordinated edit. Updated `SyncingConnectingMithrilPrompt` so both the choice and confirm views now use left-aligned Mithril prompt chrome, added the titles "Mithril Sync" and "Mithril Sync Process", merged the choice-copy body into one paragraph with sibling spans and an explicit space, split the handoff line into a separate localized `Note:` label plus body, replaced the old global `'primary'`/`actionButton` treatment with local `.primaryAction`/`.secondaryAction` tokens copied from the approved Mithril reference, normalized the confirm body/error alignment, refreshed the prompt/container Jest coverage, added confirm-view Storybook states, and regenerated the prompt SCSS module typings/i18n descriptors for the new local message keys. Narrow executable checks run for this slice: `yarn test:jest --runInBand source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx source/renderer/app/containers/loading/MithrilProactivePromptContainer.spec.tsx` (initial red before the edit, then 23 tests passing post-edit), `./node_modules/.bin/typed-scss-modules source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss`, `./node_modules/.bin/sass source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss >/tmp/task-ux-702c-cat-de.css`, `./node_modules/.bin/stylelint source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss`, targeted `eslint` on the touched prompt/spec/story files, `yarn i18n:manage`, and `yarn compile`, all passing for the slice.
- 2026-07-01 — CAT-F implemented on the shared working tree. Lifted the dead descendant-scoped `.mithrilPartialSyncConfirmation*` rules out of the top-level `.component` block in `DaedalusDiagnostics.scss`, replaced the collapsing paragraph margins with body-owned `p + p { margin-top: 20px; }`, kept the existing dialog tokens, dropped the wrapper body opacity, removed the dead `.mithrilPartialSyncConfirmationActions` / `.mithrilPartialSyncConfirmationCancelButton` selectors, added the dedicated flat `.mithrilPartialSyncConfirmationError` class, swapped `MithrilPartialSyncConfirmation.tsx` from `styles.error` to that class, refreshed the confirmation Jest assertions for the live paragraph/error hooks, re-keyed the direct Storybook confirmation stories on theme changes so the portaled dialog is easy to verify in light and dark themes, and regenerated `DaedalusDiagnostics.scss.d.ts`. Narrow executable checks run for this slice: `yarn test:jest --runInBand source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx` (initial red on the missing dialog error class, then 9 tests passing post-edit), `./node_modules/.bin/typed-scss-modules source/renderer/app/components/status/DaedalusDiagnostics.scss`, `./node_modules/.bin/sass source/renderer/app/components/status/DaedalusDiagnostics.scss >/tmp/task-ux-702c-cat-f.css`, `./node_modules/.bin/stylelint source/renderer/app/components/status/DaedalusDiagnostics.scss`, and `./node_modules/.bin/tsc --noEmit`, all passing. Operator visual proof of the restored paragraph spacing/emphasis in the running dialog remains a separate pending manual-validation item.

## Code Review

- 2026-07-01 — CAT-A review: **approved**. Reviewer confirmed the new `cancelling` contract, backend
	`cancel`/`finalizeCancel`/`forceKill`/`abandonCancel` split, coordinator bounded join, overlay cleanup state,
	store warning, Storybook proof, i18n keys, and targeted tests all match the approved plan and preserve the locked
	cancel/race invariants. Follow-up nit resolved in-doc: Boundary A in `mithril-partial-sync-prd.md` now explicitly
	names `cancelling` alongside the other pre-cutover states.
- 2026-07-01 — CAT-B review: **approved**. Reviewer confirmed the shared `.backdrop` rule stays within the locked
	styling-only slice: it reuses the existing `overlay-backrop` mixin, keeps the existing Mithril tint token, adds the
	planned `opacity: 1`, and affects only the intended shared consumers. Recorded executable validation (`sass` compile
	of `MithrilBootstrap.scss` and `stylelint` on the same file) was accepted for the per-category code-review stage;
	U2 on-device proof remains a separate operator validation gate.
- 2026-07-01 — CAT-C review: **approved**. Validation-only pass confirmed the live working tree still matches the
	locked no-code-change contract: the Diagnostics gate remains backend-owned via the AND-gate, proactive suppression
	remains coupled to `isSignificantlyBehind`, and the existing regression coverage proving that behavior remains
	unchanged.
- 2026-07-01 — CAT-D + CAT-E review: **approved**. Reviewer confirmed the shared prompt surface matches the locked
	chrome contract: both titles are present, the choice body is merged with sibling spans and explicit spacing, the
	localized `Note:` label is split out and de-dimmed, both views are left-aligned, and both button rows use the local
	Mithril `.primaryAction` / `.secondaryAction` styling. The recorded executable validation (targeted Jest, SCSS
	typings + `sass` + `stylelint`, `yarn i18n:manage`, targeted `eslint`, and `yarn compile`) was accepted for this
	per-category review stage; operator visual proof remains a separate pending manual gate.
- 2026-07-01 — CAT-F review: **approved**. Reviewer confirmed the portaled confirmation dialog selectors are now flat,
	the paragraph spacing is owned by the body `p + p` rule, the wrapper opacity is removed, the dedicated
	`.mithrilPartialSyncConfirmationError` hook is used in TSX, and the touched Storybook/spec proof surfaces match the
	locked CAT-F contract. The recorded executable validation (`typed-scss-modules`, targeted Jest, `sass`, `stylelint`,
	and `tsc --noEmit`) was accepted for this per-category review stage; the remaining visual check stays in the
	separate operator validation gate.

## Whole-task verification

- 2026-07-01 — Focused task-owned automation is green.
	- `yarn test:jest --runInBand source/main/mithril/MithrilPartialSyncService.spec.ts source/main/utils/chainStorageCoordinator.spec.ts source/renderer/app/stores/MithrilPartialSyncStore.spec.ts source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx source/renderer/app/containers/loading/MithrilProactivePromptContainer.spec.tsx source/renderer/app/components/status/MithrilPartialSyncConfirmation.spec.tsx source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx` → **PASS** (8 suites, 175 tests).
	- `yarn compile` → **PASS** (`typed-scss-modules` / `typedef:sass` pass + `tsc --noEmit` pass).
	- Targeted `eslint` across the touched TS/Storybook surfaces → **PASS with 0 errors / 50 warnings** (warnings are pre-existing style/test issues in touched files; no blocking lint errors remain).
	- Targeted `stylelint` on `MithrilBootstrap.scss`, `SyncingConnectingMithrilPrompt.scss`, and `DaedalusDiagnostics.scss` → **PASS**.
	- Touched-file `prettier --check` across the 702c docs/code/story surfaces → **PASS** after formatting the touched code/story files.
	- Editor diagnostics (`get_errors`) on the primary touched code/story surfaces → **PASS** (no errors found).

## Task-state decision

`task-ux-702c` remains **pending / not completed** because this phase-7 task is `interactive_validation` and still
awaits operator-run manual checks U1–U4 (cancel teardown latency/join-timeout, overlay blur on-device, first-load
blackout duration context, and prompt/dialog visual confirmation). `tasks.json` intentionally remains `status:
"pending"`, there is no `completedAt`, and no task commit has been created yet.