# task-ux-503 — Research notes

Captured during implementation of cross-cutting automated coverage closure for the handoff, bootstrap progress, success-finalization, and gating flows (PRD D8: #30 live-injection handoff coverage). Test-only task; no component/i18n/scss/service changes.

## No new external research required

The task was a **verified-audit + single live-injection test**. All strategy, harness anchors, and enumerated coverage locations were pre-identified and spot-checked in the grounding brief against existing source:

- **Coverage audit seams (pre-existing):** Store harness `:17-42` (mock + handler capture); App spec `:185-263` (prop-injection baseline); store tests `:158`, `:284-320` (existing push patterns).
- **Handoff gap pre-identified:** App spec uses prop-injected fake store (`shouldShowOverlay` hardcoded), never the live store path at `App.tsx:97-124` that consumes `mithrilPartialSync.shouldShowOverlay` from real store getters.
- **Genuine #30 deliverable pre-specified:** one `describe` block in `MithrilPartialSyncStore.spec.ts` reusing the existing `registeredStatusHandler` harness (`:15-42`), pushing downloading/failed/completed snapshots, and asserting the overlay-driving getter set (`shouldShowOverlay`, `status`, `filesDownloaded`, `filesTotal`, `startedAt`, recovery flags, `error`) populates live and dismiss flips the gate.
- **Recovery-branch failure-mode coverage:** pre-verified complete in `MithrilPartialSyncOverlay.spec.tsx:74,113,133,200-211` (retry/restart-normal/wipe/quit all present at failed state).

No web search, RFCs, SDK versions, or deployment research was needed. All decisions were verified against checked-in source lines.

## Implementation findings

### Live-injection harness reuse (existing pattern, no invention)
The `jest.mock('../ipc/mithrilPartialSyncChannel', ...)` at lines 17-42 captures `registeredStatusHandler` by reference. Tests call `setupStore()` (line 50) which calls `store.setup()`, registering the handler. Calling `registeredStatusHandler(snapshot)` then simulates a real backend push (the same subscriber wired at `MithrilPartialSyncStore.ts:75` in production). This is the harness the grounding brief specified; no new testing infrastructure was invented.

### Handoff contract assertions (one `describe`, 4 tests)
New block titled `'diagnostics → overlay handoff (live store contract)'` (lines 567-667) covers:

1. **Hidden by default:** Fresh store, no push → `shouldShowOverlay === false`.
2. **Downloading state flips overlay on and populates getters:** Push `status: 'downloading'`, transfer progress, empty actions → `shouldShowOverlay === true`, `status`, `filesDownloaded`, `filesTotal`, `startedAt` all live.
3. **Failed state exposes recovery flags:** Push `status: 'failed'` with `allowedRecoveryActions: ['retry', 'restart-normal', 'wipe-and-full-sync']` → all three `can*` flags true, `error` populated.
4. **Completed→dismiss→finalize seam:** Push `status: 'completed'` → overlay shown; `await store.dismissCompletedOverlay()` → `shouldShowOverlay === false` and `mockFinalizeRequest` invoked once.

Uses `jest.setSystemTime()` for deterministic `startedAt` anchor assertions (existing pattern at `:569-593`). All 25 tests (4 new + 21 pre-existing) green on focused run.

### Recovery-branch failure-mode coverage (no additions needed)
Verification of `MithrilPartialSyncOverlay.spec.tsx` confirmed:
- **retry** branch: tested at `:74-100` (rendered + clicked from failed).
- **restart-normal** branch: tested at `:113` and `:185-211`.
- **wipe-and-full-sync** branch: tested at `:204-208` (button rendered and fires).
- **quit fallback** (no recovery actions): tested at `:133-145` (defensive Quit shown when no action available).

Each branch has failure-mode (`status: 'failed'`) assertion. **No new overlay tests were required.**

### Vocabulary audit (no drift)
- Behind-ness: renderer-computed `max(1, networkTip.epoch - localTip.epoch)` only; no backend `behindByEpochs` field added; no "immutable files" string reachable.
- Gate: `isSignificantlyBehind` remains backend-owned (`MithrilPartialSyncStore.ts:123`); no renderer recomputation introduced.
- Copy: new tests use only status enum values (`'downloading'`, `'failed'`, `'completed'`) and synthetic error payload (not user-facing). No new i18n keys. Internal `'Restore failed'` message is a test fixture value, not a copy assertion.
- Deferred terminology: user-facing "Mithril Sync" rollout is `task-ux-601`, not 503. The literal "Mithril partial sync" in diagnostics namespace is acceptable per guardrails.

No copy assertions were added or modified; vocabulary guardrails held intact.

### Test determinism and env notes
- Uses `jest.useFakeTimers()` (module-level, line 6) with safe `jest.setSystemTime(100_000)` anchor.
- Nested `beforeEach` sets `mockStatusRequest.mockResolvedValue({ status: 'idle' })` to bypass `electronLog` (unavailable in jsdom) on `store.setup()`'s async path.
- `afterEach(jest.clearAllTimers())` already present (`:62-64`); no new timer leaks.
- No scss imports; `typed-scss-modules` workaround not needed.
- Per memory, store spec does not import scss, so plain `yarn test:jest <path>` passes without identity-obj-proxy sidecar.

All 25 tests consistently green across focused and full runs. No flakiness, no env caveat triggering.

## Scribe finalization (2026-06-26)

No new research surfaced at finalize beyond the above. Plan-review APPROVED (2026-06-26T15:26:42Z) and impl-review APPROVED (2026-06-26T15:02:02Z). `mithril-partial-sync-ux-refinement-tasks.json` `task-ux-503` → `completed`, `completedAt` `2026-06-26`. Durable findings carried forward: the handoff gap was prop-injection in App spec, not live-store-path exercise; the harness reuse (existing `registeredStatusHandler` capture pattern) satisfied #30 with zero new infrastructure; recovery-branch coverage already complete from prior overlay tests (no new overlay tests needed); vocabulary guardrails all held (epochs-only behind-ness, no "immutable files", backend-owned gate, deferred user-facing copy rollout to task-ux-601).
