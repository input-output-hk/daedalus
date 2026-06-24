# task-ux-301 — Implementation & Code Review Log

Append-only. Each entry: speaker label, ISO-8601 UTC Timestamp, outcome.
Critiquer/Code-Review entries end with "Decision: approved" or "Decision: requires_changes".

---

Implementation:
Timestamp: 2026-06-24T15:50:00Z

Implemented the approved plan end to end. No new IPC channel, no threshold math, no 302/303/404 work.

Files changed (source):
- `source/renderer/app/stores/MithrilPartialSyncStore.ts` — added type import
  `MithrilPartialSyncAvailability` and value import `mithrilPartialSyncAvailabilityChannel`; module
  const `AVAILABILITY_REFRESH_INTERVAL = 30_000`; three `@observable` fields (`isPartialSyncEnabled`
  default `false`, `isSignificantlyBehind` default `false`, `behindByImmutables` default `undefined`)
  + `_availabilityRefreshInterval` field; `@action _refreshAvailability` (async, `_isTornDown`-guarded,
  swallows its own rejection via `logger.warn`) and `@action _applyAvailability` (`_isTornDown`-guarded
  setter — stores `behindByImmutables` verbatim, NO threshold comparison); `setup()` now fires the
  one-shot `_refreshAvailability()` and starts a `setInterval` that refreshes ONLY while `this.isWorking`;
  `teardown()` clears the interval before setting the guard / calling `super.teardown()`.
- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` — feed the partial-sync block
  signal from `mithrilPartialSync.isWorking` (renamed prop `isMithrilPartialSyncWorking`, NOT `isActive`);
  thread two new props `isMithrilPartialSyncEnabled={mithrilPartialSync.isPartialSyncEnabled}` and
  `isMithrilPartialSyncSignificantlyBehind={mithrilPartialSync.isSignificantlyBehind}`.
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` — Props: replaced
  `isMithrilPartialSyncActive` with `isMithrilPartialSyncWorking` + added the two availability props;
  render destructure updated; `isMithrilActionBlocked` now derives from `isMithrilPartialSyncWorking ||
  isMithrilBootstrapActive` (the `|| isMithrilBootstrapActive` serialized-lock half preserved);
  `<MithrilPartialSyncSection>` is rendered ONLY when `isMithrilPartialSyncEnabled` and is passed
  `shouldShowRecommendation={isMithrilPartialSyncEnabled && isMithrilPartialSyncSignificantlyBehind}`.
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` — Props: renamed
  `isMithrilPartialSyncActive` → `isMithrilPartialSyncWorking`, added `shouldShowRecommendation`;
  `componentDidUpdate` uses the renamed prop; `render()` destructures `shouldShowRecommendation` and,
  AFTER the existing `isShowingConfirmation` early-return and BEFORE the `layoutRow` JSX, returns `null`
  when `!shouldShowRecommendation` — kills the empty labeled "Mithril Partial Sync:" header row near the
  tip (Critiquer blocker #1).
- `source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx` — UNCHANGED (READ-ONLY
  per plan; gate enforced one level up in the Section).
- `source/common/types/mithril-partial-sync.types.ts` — UNCHANGED (READ-ONLY; the
  `MithrilPartialSyncAvailability` type and `isMithrilPartialSyncWorkingStatus` already exist).

Deviation from plan (single, mechanical, logged):
- `storybook/stories/nodes/status/Diagnostics.stories.tsx` was NOT in the plan's targetPaths/file list,
  but it passed the now-renamed `isMithrilPartialSyncActive: false` arg to `DaedalusDiagnostics`. The
  story is cast `as any`, so `tsc` did not catch it, but after the rename the story would have rendered
  with `isMithrilPartialSyncEnabled` undefined → the partial-sync Section (and the
  `PartialSyncConfirmationStory` it demos) hidden — a silent story regression caused by my rename.
  Updated the three args to match (`isMithrilPartialSyncWorking: false`,
  `isMithrilPartialSyncEnabled: true`, `isMithrilPartialSyncSignificantlyBehind: true`). This is a
  mechanical follow-through of the prop rename, not a product/UX/copy decision and not 302/303/404
  scope; logged here for traceability.

Tests added/updated (mapped to the 4 JSON testCases):
- `MithrilPartialSyncStore.spec.ts` — added `mithrilPartialSyncAvailabilityChannel` mock + a default
  `mockAvailabilityRequest.mockResolvedValue({ isEnabled: true, isSignificantlyBehind: false })` in
  `beforeEach` (after `jest.resetAllMocks()`). New tests: (1) one-shot availability consumption at setup
  populates `isPartialSyncEnabled`/`isSignificantlyBehind`/`behindByImmutables=42`, called once;
  (2) fail-closed defaults before first response (all hidden); (3) interval refreshes ONLY while working
  (idle advance → still 1 call; push `downloading` + advance → 2 calls); (4) teardown clears the interval
  (advance 90s post-teardown → still 1 call); (5) testCase (c) store layer — `completed`/`cancelled`/
  `failed` all yield `isWorking === false` && `isTerminal === true`.
- `DaedalusDiagnostics.spec.tsx` — `defaultProps`: `isMithrilPartialSyncActive` → `isMithrilPartialSyncWorking:
  false`, added `isMithrilPartialSyncEnabled: true`, `isMithrilPartialSyncSignificantlyBehind: true`.
  testCase (d) renamed/repointed to `isMithrilPartialSyncWorking: true` (disabled + hint); testCase (c)
  wired layer (`isMithrilPartialSyncWorking: false` → button enabled); testCase (a) kill-switch off
  (`isMithrilPartialSyncEnabled: false` → no button AND `queryByText(/Mithril Partial Sync/)` null);
  testCase (b) enabled && significantlyBehind shows button, then rerender significantlyBehind=false →
  button + header both absent (regex matcher, per plan). Bootstrap-active test retained.
- `MithrilPartialSyncSection.spec.tsx` — `defaultProps`: renamed prop + added
  `shouldShowRecommendation: true`; renamed the bare `isMithrilPartialSyncActive` prop in the
  external-becomes-active test to `isMithrilPartialSyncWorking`; added a gated-off test asserting the
  whole Section renders nothing (`container.textContent` does NOT match `/Mithril Partial Sync/` and no
  button) when `shouldShowRecommendation={false}`.
- `DaedalusDiagnosticsDialog.spec.ts` — imported the live `isMithrilPartialSyncWorkingStatus`; added
  `isWorking: false`, `isPartialSyncEnabled: true`, `isSignificantlyBehind: true` to the mock store and
  `isWorking: isMithrilPartialSyncWorkingStatus(status)` to `withStatus` (kept the harmless `isActive`
  mock field).

`_isTornDown` teardown-guard documentation (acceptance requirement, research-19 row #15):
The `onReceive` status subscription registered in `setup()` is STILL not unsubscribed — the
`_isTornDown` guard remains the inert-handler mechanism (`_updateStatus`/`syncStatus` already check it,
and the new `_refreshAvailability`/`_applyAvailability` both check it too, so a late-resolving
`request()` is inert after teardown). NO `removeListener`/IPC-abstraction unsubscribe was added; this
task adds ZERO new IPC. The ONE thing cleaned up in `teardown()` is the new plain `setInterval` we own
(`_availabilityRefreshInterval`), which is cleared and nulled before `super.teardown()`.

Verification (run from repo root `/workspaces/mithril-partial-sync-ux`):
- `tsc --noEmit` (the whole-project gate `yarn compile` ultimately runs): PASS, 0 errors, exit 0.
  NOTE: `yarn compile` itself could not complete because its `precompile` hook `typedef:sass`
  (`typed-scss-modules`) crashes loading the bundled dart-sass 1.44.0 bundle under Node v24 in this
  sandbox (`SyntaxError: Invalid or unexpected token` in `node_modules/sass/sass.dart.js`). This is a
  pre-existing ENVIRONMENT defect unrelated to this change; the real TS gate (`tsc --noEmit`, which is
  exactly what `"compile"` is) was run directly and PASSES.
- `eslint --ext .ts,.tsx <changed files>` (5 source + 4 spec + the storybook file): exit 0, 0 errors,
  53 warnings — all pre-existing/stylistic (`@ts-ignore`, `no-explicit-any` on `...args: Array<any>` /
  `as any` test casts, non-null assertions in pre-existing tests, and the spurious decorator-import
  "unused" warnings for `action`/`computed`/`observable`). No NEW errors introduced.
- `yarn test:jest`:
  - `MithrilPartialSyncStore.spec.ts`: PASS, 15/15 (10 pre-existing + 5 new), via the project jest
    runner directly (no `.scss` import in the store path, so it runs unaffected).
  - `DaedalusDiagnostics.spec.tsx`, `MithrilPartialSyncSection.spec.tsx`, `DaedalusDiagnosticsDialog.spec.ts`:
    these three import `.scss`, so under the project runner they FAIL TO RUN at the transform layer
    ("Can't find sass or node-sass module") — the SAME pre-existing dart-sass/Node-v24 environment
    defect above, NOT a test failure. To prove the test LOGIC, the corrupt 86-byte
    `node_modules/sass/sass.default.dart.js` shim entry was temporarily replaced with a minimal
    `renderSync` stub returning empty CSS (these specs assert on ARIA roles/text, not CSS class values),
    the three specs ran GREEN — 18/18 passed — and the original `sass.default.dart.js` was restored
    byte-for-byte (verified 86 bytes, identical content). `node_modules` left exactly as found.

Bootstrap-regression assessment: NONE. This task did not touch any shared bootstrap progress component
(`MithrilProgressView`, `MithrilStepIndicator`, `MithrilBootstrapStore`) or the empty-chain bootstrap
flow (lock #11). The bootstrap term in `isMithrilActionBlocked`
(`|| isMithrilBootstrapBlockingNodeStart(mithrilBootstrap.status)`) is unchanged; the existing
"CTA disabled while bootstrap work active" test still passes.

User handoff: none required. Branch-scoped kill switch already true (task-ux-101); production default /
network-scoping remain deferred to the phase-7 readiness gate.

Outcome: implemented as planned (one logged mechanical storybook follow-through); awaiting code review.

---

Code Review:
Timestamp: 2026-06-24T15:53:23Z

Reviewed the working-tree diff against the approved plan (task-ux-301.md), the four acceptance
criteria, the four testCases, and the locked invariants. Independently re-ran the decisive checks.

Diff vs plan — exact match on all five edited source files:
- `MithrilPartialSyncStore.ts`: type import `MithrilPartialSyncAvailability` + value import
  `mithrilPartialSyncAvailabilityChannel`; `AVAILABILITY_REFRESH_INTERVAL = 30_000`; three observables
  (`isPartialSyncEnabled=false`, `isSignificantlyBehind=false`, `behindByImmutables=undefined`) +
  `_availabilityRefreshInterval`; `@action _refreshAvailability` (async, `_isTornDown`-guarded, swallows
  rejection via `logger.warn`) and `@action _applyAvailability` (`_isTornDown`-guarded, stores
  `behindByImmutables` verbatim); `setup()` one-shot + interval refreshing ONLY while `this.isWorking`;
  `teardown()` clears+nulls the interval before the guard/super. Matches Steps 1-6 byte-for-byte.
- Container `DaedalusDiagnosticsDialog.tsx`: feeds `isMithrilPartialSyncWorking={mithrilPartialSync.isWorking}`
  (NOT `isActive`) + threads `isMithrilPartialSyncEnabled`/`isMithrilPartialSyncSignificantlyBehind`.
- `DaedalusDiagnostics.tsx`: Props + destructure renamed/extended; `isMithrilActionBlocked =
  isMithrilPartialSyncWorking || isMithrilBootstrapActive` (serialized-lock half preserved); Section
  rendered ONLY when `isMithrilPartialSyncEnabled`, passed `shouldShowRecommendation = isEnabled &&
  isSignificantlyBehind`.
- `MithrilPartialSyncSection.tsx`: prop rename; `componentDidUpdate` uses renamed prop; render returns
  `null` AFTER the `isShowingConfirmation` early-return when `!shouldShowRecommendation` (kills the empty
  labeled header row near the tip).

Acceptance criteria — all verified:
(a) Kill switch off hides ALL partial-sync UI: Section is `{isMithrilPartialSyncEnabled && (…)}`; default
    `isPartialSyncEnabled=false` is fail-closed (lock #10). No enabled button reachable when off — confirmed
    by the green `DaedalusDiagnostics` test asserting both `queryByRole('button', {name:'Mithril Partial
    Sync'})` and `queryByText(/Mithril Partial Sync/)` are null.
(b) Recommendation/CTA only when `isEnabled && isSignificantlyBehind`: threaded `shouldShowRecommendation`;
    Section `null`-returns its non-confirmation row when the gate is false (D1 hide-not-disable, no residual
    header). Green test toggles `significantlyBehind` true→false and asserts header + button both vanish.
(c) Terminal `completed`/`cancelled`/`failed` re-arm the CTA: `isActionBlocked` now derives from
    `isWorking` (in-flight phases only). Store test confirms all three terminal statuses yield
    `isWorking===false`; component test confirms button `toBeEnabled()` when not working (D9/BUG1).
(d) CTA stays disabled while other Mithril work active: serialized-lock `|| isMithrilBootstrapActive` half
    PRESERVED unchanged; `isMithrilPartialSyncWorking:true` and `isMithrilBootstrapActive:true` tests both
    assert the disabled button + hint (lock #14).

Locked invariants:
- #4 no renderer threshold: grep confirms `behindByImmutables` is ONLY assigned verbatim
  (`MithrilPartialSyncStore.ts:192`), never compared to any number anywhere in `source/renderer/`. PASS.
- #10 kill-switch hides all UI: PASS (see (a); fail-closed default).
- D9/BUG1: PASS (`isWorking` derivation; getter `isActive` and helper `isMithrilPartialSyncActiveStatus`
  left INTACT — store getter still at `:96-97`, helper still at types `:98-100`; only the CTA-block path
  migrated). No `isActive`-fed prop remains in the renderer (grep: zero `isMithrilPartialSyncActive` prop
  references in `source/renderer/` or `storybook/`).
- #15 `_isTornDown` guard: both new actions guarded; `onReceive` subscription remains guard-inert with NO
  IPC unsubscribe added; the new plain `setInterval` IS cleared in `teardown()`. Documented (impl-review
  lines 81-87 + plan Step 6 + Locked invariants). PASS.
- #24 retry=start: documented in plan Risks (`:695-697`) and plan-review. PASS.
- Scope: no shared bootstrap component touched (lock #11), no overlay/finalize/dismiss touched (404 scope),
  no new IPC channel and no type-file edit (zero new IPC; types READ-ONLY). All confirmed by name-only diff.

Independent verification (re-run from repo root, real output):
- `tsc --noEmit`: PASS, exit 0 (this is the `yarn compile` gate).
- `jest MithrilPartialSyncStore.spec.ts`: 15/15 PASS (5 new).
- `jest MithrilPartialSyncSection.spec.tsx`: 7/7 PASS. `jest DaedalusDiagnostics.spec.tsx
  DaedalusDiagnosticsDialog.spec.ts`: 11/11 PASS. NOTE: the three `.scss`-importing specs ran GREEN under
  the project runner in THIS review environment (the dart-sass/Node-v24 transform defect the implementer
  hit did not reproduce here; either way the test LOGIC is proven green, 33/33 across all four suites).
- Storybook deviation (`Diagnostics.stories.tsx` args updated to the renamed/added props) is a correct,
  mechanical follow-through of the prop rename, not 302/303/404 scope — accepted.

No blockers. The change is the smallest truthful implementation of the approved plan, invariant-safe, and
all four testCases pass independently.

Decision: approved
