# task-ux-503 — Close cross-cutting automated coverage (handoff, bootstrap progress, success-finalization, gating)

## Task id + title
`task-ux-503` — "Close cross-cutting automated coverage (handoff, bootstrap progress, success-finalization, gating)".

## Why now
Test-only convergence task. All enumerated feature behaviors already shipped with focused tests in
prior per-feature tasks (`task-ux-3xx`/`4xx`). This task's job is **not** to re-author that coverage,
but to (a) verify it exists, (b) close the one genuine gap, and (c) guarantee automated verification
can run before manual QA begins.

## Interaction mode
`autonomous` — test authoring only. No user-blocking decision. No new user-facing copy, no new i18n
keys, no component/store/service production changes.

## Acceptance criteria (from tasks.json, verbatim)
- Cross-cutting automated coverage exists for handoff, bootstrap progress, gating, success
  finalization, live activity affordances, and recovery actions.
- At least one failure mode is covered for each recovery-option branch.
- Automated verification can run before manual QA begins.

## Convergence finding (THE core decision — read first)
Per the prompt's convergence rule (smallest truthful change; reuse existing seams), an audit found
that **almost every enumerated coverage item ALREADY EXISTS**. DO NOT duplicate it. Verified anchors:

| Coverage area | Already covered at |
|---|---|
| gating / availability | `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx:127,136`; `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts:436,461,469,501` |
| success reset-to-idle + staging cleanup (D9) | `source/main/mithril/MithrilPartialSyncService.spec.ts:1087-1151` |
| CTA re-arm | `MithrilPartialSyncStore.spec.ts:552`; `DaedalusDiagnostics.spec.tsx:119` |
| elapsed ticks via renderer timer | `source/renderer/app/components/loading/mithril-bootstrap/MithrilProgressView.spec.tsx:45,79` |
| animated active step | `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx:39` |
| stopping-node frame | `MithrilStepIndicator.spec.tsx:72`; `MithrilProgressView.spec.tsx:67` |
| cancel-disabled-during-stop | `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx:155` |
| resync-on-cancel | `MithrilPartialSyncStore.spec.ts:237`; `MithrilPartialSyncService.spec.ts:799` |
| error-copy mapping | `source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.spec.ts` (whole file); `MithrilPartialSyncOverlay.spec.tsx:214,239` |
| recovery actions from `allowedRecoveryActions` | `MithrilPartialSyncOverlay.spec.tsx:74,113,133` |
| D11/D12 epochs behind-ness | `DaedalusDiagnostics.spec.tsx:185,200,210`; `MithrilPartialSyncConfirmation.spec.tsx:54,73`; `MithrilPartialSyncSection.spec.tsx:231` |
| bootstrap file-count progress | `MithrilStepIndicator.spec.tsx:173,194`; `source/renderer/app/stores/MithrilBootstrapStore.spec.ts:54` |

These were spot-checked (store harness `:17-42`, store `:158`, `:284-320`, App `:185-263`) and confirmed.
**Trust the audit; do not re-author any of the above.**

## The ONE net-new deliverable — research-19 row #30: live-injection diagnostics → overlay handoff
### The gap
`source/renderer/app/App.spec.tsx:185-263` mounts the overlay by injecting a **plain-object fake**
store with `shouldShowOverlay` hardcoded `true` and `filesDownloaded`/`status`/recovery flags as
literal props (`App.spec.tsx:200-219`). That is **prop injection**, not live injection — it never
exercises the real store getters that production actually consumes.

The production handoff seam:
- `source/renderer/app/App.tsx:97-124` renders `<MithrilPartialSyncOverlay>` **only** when
  `mithrilPartialSync.shouldShowOverlay`, and feeds it entirely from live store getters
  (`status`, `startedAt`, `transferProgress` → `filesDownloaded`/`filesTotal`, recovery flags).
- `source/renderer/app/stores/MithrilPartialSyncStore.ts:75` —
  `mithrilPartialSyncStatusChannel.onReceive(async (update) => { this._updateStatus(update); })` is
  the **live backend-push entry point**.
- `:104` `get isWorking`; `:119-120` `get shouldShowOverlay { return this.hasDisplayStatus && !this.isCompletedOverlayDismissed }`;
  `:66` `@observable isCompletedOverlayDismissed=false`; `:229` set true on dismiss.

### Harness to REUSE (do NOT invent a new one)
`MithrilPartialSyncStore.spec.ts:15-42` already captures the live push handler:
```ts
let registeredStatusHandler;
jest.mock('../ipc/mithrilPartialSyncChannel', () => ({
  mithrilPartialSyncStatusChannel: {
    request: (...args) => mockStatusRequest(...args),
    onReceive: (handler) => { registeredStatusHandler = handler; },
  },
  // ...other channels
}));
```
Calling `registeredStatusHandler(snapshot)` simulates a real backend push (this is exactly what
`onReceive` wires in production via `setup()`). The spec already uses this pattern at
`:284-320` and `:377-389`, and `setupStore()` (`:50`) calls `store.setup()` so the handler is
registered. Note `store._updateStatus(...)` is the thin internal applied-by-the-handler path; the
**handoff test must go through `registeredStatusHandler(...)`** to exercise the live seam, not call
`_updateStatus` directly.

### Where to add the test
Add **one new focused `describe`** block to
`source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` titled approximately
`'diagnostics → overlay handoff (live store contract)'`. This is where the live harness already
lives and where the convention fits best. Do **not** add a parallel fake-store path to `App.spec.tsx`
(that file's existing prop-injection test already proves App.tsx forwards getters → props/callbacks;
the missing half is "the real store getters populate from a live push", which belongs in the store
spec). A thin App-level live variant is optional and NOT required — prefer the single store-spec
describe to keep the change smallest.

### Exact assertions (the handoff contract)
Use `setupStore()` for a REAL `MithrilPartialSyncStore`, then drive `registeredStatusHandler`:

1. **Hidden by default.** Fresh store (no push yet): `expect(store.shouldShowOverlay).toBe(false)`
   (status is the idle default; `hasDisplayStatus` false).

2. **Downloading push flips overlay on and populates the overlay-driving getters.**
   Push:
   ```ts
   registeredStatusHandler({
     status: 'downloading',
     allowedRecoveryActions: [],
     transferProgress: { filesDownloaded: 4, filesTotal: 9, elapsedSeconds: 12 },
     progressItems: [],
     error: null,
   });
   ```
   Assert the exact set App.tsx:97-124 consumes:
   - `expect(store.shouldShowOverlay).toBe(true)`
   - `expect(store.status).toBe('downloading')`
   - `expect(store.filesDownloaded).toBe(4)` and `expect(store.filesTotal).toBe(9)`
   - `expect(store.startedAt).not.toBeNull()` (renderer-anchored on entering a working status;
     with fake timers set via `jest.setSystemTime(...)` you may assert the exact anchored value
     `now - elapsedSeconds*1000`, matching the existing `:569-593` startedAt tests).

3. **Failed push exposes recovery flags live.**
   ```ts
   registeredStatusHandler({
     status: 'failed',
     allowedRecoveryActions: ['retry', 'restart-normal', 'wipe-and-full-sync'],
     transferProgress: {},
     progressItems: [],
     error: { message: 'Restore failed', stage: 'installing' },
   });
   ```
   Assert: `shouldShowOverlay === true`, `status === 'failed'`,
   `canRetry === true`, `canRestartNormally === true`, `canWipeAndFullSync === true`,
   and `error` populated. (This is the live half of the recovery-branch contract; the
   button-render/callback half is already covered in `MithrilPartialSyncOverlay.spec.tsx:74,113,133,200-211`.)

4. **Completed push keeps overlay shown, then dismiss flips it off (success finalization seam).**
   Push `status: 'completed'` (allowedRecoveryActions `[]`); assert `shouldShowOverlay === true`.
   Then `await store.dismissCompletedOverlay()`; assert `shouldShowOverlay === false` and that the
   finalize channel was invoked (`expect(mockFinalizeRequest).toHaveBeenCalledTimes(1)` — mock it
   `mockResolvedValue(undefined)` as `:158` does). This proves `isCompletedOverlayDismissed`
   (`store.ts:66,229`) gates the live getter, mirroring App.tsx's `onDismissCompleted` wiring.

> Note: `MithrilPartialSyncStore.spec.ts:158` ("shows the overlay only for backend-confirmed display
> states and can dismiss completed") covers the dismiss behavior but drives it through
> `_updateStatus`, not the live `registeredStatusHandler` push, and does not assert the
> overlay-driving getter set (`filesDownloaded`/`startedAt`/recovery flags) as a single handoff
> contract. The new describe makes the **live push → getter population** contract explicit. If during
> implementation the new describe would be a verbatim duplicate of `:158`, prefer extending #30 with
> the getter-population + live-push assertions rather than copying.

### Recovery-branch failure-mode audit (AC #2)
Confirm at least one failure-mode assertion exists for EACH recovery-option branch and add only what
is genuinely missing:
- **retry** — `MithrilPartialSyncOverlay.spec.tsx:74,113` (rendered + clicked from `failed`).
- **restart-normal** — `MithrilPartialSyncOverlay.spec.tsx:113,202`.
- **wipe-and-full-sync** — `MithrilPartialSyncOverlay.spec.tsx:133,204-208`.
- **quit fallback (no recovery actions)** — `MithrilPartialSyncOverlay.spec.tsx:133` region
  (defensive Quit appears only when no action is available).
All four branches already have failure-mode (`status: 'failed'`/`cancelled`) coverage. **Expected
outcome: nothing to add here.** Only if one branch lacks a `failed`-state assertion, add a single
focused `it` in `MithrilPartialSyncOverlay.spec.tsx` using the existing `renderComponent(overrides)`
helper. Do not refactor the existing tests.

## Vocabulary guardrails (must hold in any assertion this task adds)
- Behind-ness is **epochs only**, renderer-computed as `max(1, networkTip.epoch - localTip.epoch)`.
  Any copy assertion must use epochs; **no "immutable files" string may reach the user**.
- The gate `isSignificantlyBehind` stays **backend-owned** — do not assert renderer recomputes it.
- The literal `"Mithril partial sync"` inside the **diagnostics namespace**
  (`DaedalusDiagnostics.spec.tsx:81,90,195`) is acceptable; the user-facing "Mithril Sync" rollout is
  `task-ux-601`, **NOT** 503. Do **not** "fix" copy here.
- This task adds **no** new i18n keys and **no** user-facing copy. The #30 test asserts store
  getters/booleans, not copy, so it has no vocabulary surface beyond status string literals
  (`'downloading'`, `'failed'`, `'completed'`), which are internal enum values, not user copy.

## Non-goals / scope guard
- **No** production source changes (no `MithrilPartialSyncStore.ts`, `App.tsx`, components, services).
- **No** re-authoring of the already-covered items in the table above.
- **No** new i18n keys, **no** scss changes, **no** Storybook (that was `task-ux-502`), **no**
  Cucumber feature/step changes.
- **No** `git add -A`; do **not** commit the pre-existing `.gitignore` change.
- Keep the diff to (most likely) a single file: `MithrilPartialSyncStore.spec.ts`. Touch
  `MithrilPartialSyncOverlay.spec.tsx` **only** if the recovery-branch audit finds a genuine hole.

## Spec harness conventions to follow (match the file you edit)
Renderer store spec (`MithrilPartialSyncStore.spec.ts`):
- `jest.useFakeTimers()`; module-level mock fns per IPC channel; `jest.mock('../ipc/mithrilPartialSyncChannel', ...)`.
- `beforeEach(() => { jest.resetAllMocks(); registeredStatusHandler = undefined; })`;
  `afterEach(() => jest.clearAllTimers())` (see `:53-99`).
- Use `setupStore()` (`:50`) to get a real store with `setup()` already called (registers the handler).
- For `startedAt` exactness, set `jest.setSystemTime(...)` before the push (pattern at `:569-593`).
- Mock `mockFinalizeRequest.mockResolvedValue(undefined)` before asserting dismiss.

Renderer component spec (only if editing `MithrilPartialSyncOverlay.spec.tsx`):
- `IntlProvider locale="en-US"` with messages from `en-US.json`; `@testing-library/react`
  render/screen/cleanup; `jest.mock` react-polymorph `PopOver`; `defaultProps` + `renderComponent(overrides)`
  helper; `afterEach(cleanup)`; assert via `screen.getByRole` / exact copy strings.

## Verification plan (AC #3 — automated verification runs before manual QA)
- Focused: `yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`
  (and `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`
  only if touched).
- Full regression: `yarn test:jest`.
- `yarn compile` (tsc --noEmit). This is test-only and touches no scss, so the typed-scss-modules
  workaround should NOT be needed; apply it only if tsc trips on a touched `.scss`
  (`node_modules/.bin/typed-scss-modules <file.scss>`).
- `yarn lint`; `yarn prettier:check`.
- Env caveat (Node v24): per memory, focused jest on scss-importing renderer specs may need the
  gitignored identity-obj-proxy scss sidecar `--config`; `jest.config.js` already maps scss (`:203`)
  and identity-obj-proxy (`:91`) for full runs. The store spec does not import scss, so a plain
  `yarn test:jest <path>` should pass. Do not treat a pre-existing env caveat as a regression.

## Files expected to change
- `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` (add one focused `describe` for #30).
- (Conditional, only if audit finds a hole) `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx`.
- Docs in this folder (this plan + reviews + research per the execution loop).

## Test cases (net-new)
1. Fresh live store hides the overlay (`shouldShowOverlay === false`).
2. Live `downloading` push flips `shouldShowOverlay` true and populates
   `status`/`filesDownloaded`/`filesTotal`/`startedAt`.
3. Live `failed` push exposes `canRetry`/`canRestartNormally`/`canWipeAndFullSync` + `error`.
4. Live `completed` push keeps overlay shown; `dismissCompletedOverlay()` flips it false and calls
   the finalize channel once.
5. (Conditional) any recovery branch missing a `failed`-state failure-mode assertion in the overlay
   spec gets exactly one focused `it`.

## Risks / open questions
- **Duplication risk vs `:158`/`:284-320`.** Mitigation: the new describe asserts the **full
  overlay-driving getter set as one handoff contract via the live `registeredStatusHandler` push**,
  which neither existing test does together. If implementation finds it would be a verbatim
  duplicate, fold the getter-population assertions into the closest existing live-push test instead
  of adding a redundant block — still satisfies #30.
- **`startedAt` flakiness.** Mitigation: use `jest.setSystemTime` before the push (existing pattern);
  do not assert wall-clock.
- Recovery-branch audit expected to find nothing missing; the conditional file stays untouched.

## Required doc / research updates
- This plan (`task-ux-503.md`).
- `task-ux-503-plan-review.md` (critique verdict).
- `task-ux-503-impl-review.md` (code-review verdict).
- `task-ux-503-research.md` ("no new research" — the live-injection seam and all anchors are
  pre-existing and were verified against source; no external research required).

## Review-log paths
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-5/task-ux-503-plan-review.md`
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-5/task-ux-503-impl-review.md`
- `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-5/task-ux-503-research.md`

## Commit
Exactly ONE commit, type `test(mithril):`, e.g.
`test(mithril): task-ux-503 add live-injection diagnostics→overlay handoff coverage`.
Do not stage the pre-existing `.gitignore` change; commit only the spec (+docs).

---

Planner: Audited the enumerated cross-cutting coverage and confirmed (spot-checked store harness `:17-42`, store `:158`/`:284-320`, App `:185-263`) that handoff-adjacent, bootstrap-progress, gating, success-finalization, live-activity, recovery-action, and epochs-behind coverage ALL already exist from prior per-feature tasks — so the convergent smallest-change is NOT to duplicate them. The single genuine gap is research-19 row #30: today's `App.spec.tsx:200-219` is prop injection of a hardcoded fake store, never the live store path `App.tsx:97-124` consumes. Decision: add ONE focused `describe` to `MithrilPartialSyncStore.spec.ts` that instantiates the REAL store, pushes downloading/failed/completed snapshots through the existing `registeredStatusHandler` (harness `:15-42`), and asserts `shouldShowOverlay` flips true with the overlay-driving getters (status, filesDownloaded/filesTotal, startedAt, recovery flags, error) populated, plus dismiss→finalize flips it false. Recovery-branch failure-mode coverage already complete in `MithrilPartialSyncOverlay.spec.tsx:74,113,133,200-211` (retry/restart-normal/wipe/quit) — expected nothing to add. Vocabulary guardrails: epochs-only behind-ness, no "immutable files", backend-owned `isSignificantlyBehind`, no copy "fixes" (deferred to 601). Verify with focused + full `yarn test:jest`, `yarn compile`, `yarn lint`, `yarn prettier:check`. One `test(mithril):` commit; do not stage `.gitignore`.

Scribe: 2026-06-26 — Added one focused `describe('diagnostics → overlay handoff (live store contract)')` block to `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts` (lines 567-667) with 4 tests exercising the real store getters (`shouldShowOverlay`, `status`, `filesDownloaded`, `filesTotal`, `startedAt`, recovery flags, `error`) across the downloading/failed/completed→dismiss→finalize live-push contract. Plan-review APPROVED (2026-06-26T15:26:42Z); impl-review APPROVED (2026-06-26T15:02:02Z). Recovery-branch failure-mode coverage verified complete in overlay spec (retry/restart-normal/wipe/quit all tested at failed state). Vocabulary audit passed (epochs-only, no "immutable files", backend-owned gate, no copy drifts). All 25 tests green focused + full. Commit staged and ready. Zero new i18n, scss, or component changes.
