# task-ux-503 — Implementation Review

**Reviewer:** Code Reviewer agent
**Date:** 2026-06-26
**Verdict:** APPROVED

---

## Diff summary

Single file change: `source/renderer/app/stores/MithrilPartialSyncStore.spec.ts`

Adds a new `describe('diagnostics → overlay handoff (live store contract)')` block containing 4 focused tests (lines 567-667 after patch). No production code is modified. No other spec files are touched.

---

## Criteria evaluation

### 1. Adds genuine #30 live-injection coverage — PASS

The grounding brief identified one remaining gap: App.spec.tsx uses a prop-injected fake store (hardcoded `shouldShowOverlay`), so the live IPC → store → overlay-getter chain was untested.

The new describe block:
- Instantiates the **real** `MithrilPartialSyncStore` via the existing `setupStore()` helper.
- Calls `store.setup()`, which registers the IPC `onReceive` handler into the module-level `registeredStatusHandler` captured in the existing `jest.mock` at lines 17-42.
- Pushes snapshots via `registeredStatusHandler(snapshot)` — the same code path that `mithrilPartialSyncStatusChannel.onReceive` drives at runtime (`MithrilPartialSyncStore.ts:75`).
- Asserts `shouldShowOverlay`, `status`, `filesDownloaded`, `filesTotal`, `startedAt`, recovery flags (`canRetry`, `canRestartNormally`, `canWipeAndFullSync`), `error`, and dismiss/finalize sequencing.

This exercises exactly the live store path that `App.tsx:97-124` consumes.

### 2. No duplication of existing tests — PASS

The pre-existing test at `:158` (`'shows the overlay only for backend-confirmed display states and can dismiss completed'`) uses `store._updateStatus()` (private method call), bypassing the IPC subscriber. The new tests use `registeredStatusHandler()`. These are distinct paths; no duplication.

No new tests replicate any of the existing coverage enumerated in the grounding brief (gating, elapsed ticks, step indicator animation, cancel-disabled-during-stop, resync-on-cancel, error-copy mapping, etc.).

### 3. Determinism — PASS

- Uses `jest.useFakeTimers()` (module-level, line 6) and `jest.setSystemTime(100_000)` for the `startedAt` derivation test.
- Nested `beforeEach` sets a safe `mockStatusRequest.mockResolvedValue({ status: 'idle' })` so `store.setup()`'s async `syncStatus()` path does not reach `electronLog` (unavailable in jsdom). Avoids flakiness.
- `afterEach(jest.clearAllTimers())` (line 62-64) already in outer block; no new timers are introduced.
- All 25 tests green on focused run, 0 failures.

### 4. Recovery-option branch failure-mode coverage — PASS

Grounding criterion: at least one failure-mode assertion per recovery branch. Verified in `MithrilPartialSyncOverlay.spec.tsx`:

| Branch | Failure-mode present | Location |
|---|---|---|
| `canRetry` true/fires | Yes | :74-100, :185-211 |
| `canRetry` false / button absent | Yes (implicitly via defaultProps `canRetry: false` + Quit shown instead) | :133-153 |
| `canRestartNormally` true/fires | Yes | :74-100, :185-211 |
| `canWipeAndFullSync` false / button absent | Yes | :74-100 (`canWipeAndFullSync: false`, query returns null at :95-99) |
| `canWipeAndFullSync` true/fires | Yes | :185-211 |
| Quit fallback (no actions) | Yes | :133-145 |
| Quit hidden when action present | Yes | :146-153 |

New handoff test: `'live failed push exposes all three recovery flags'` asserts all three flags true simultaneously via live push — adds live-injection coverage of the combined-flags path without duplicating overlay rendering assertions.

### 5. No forbidden vocabulary — PASS

Diff contains no "immutable files" string. No user-facing copy assertions introduced. The string `'Restore failed'` in the new test is a synthetic error `message` field value (internal backend payload), not a user-facing copy assertion.

### 6. No copy drift / epochs-only — PASS

No behind-ness or epoch copy assertions are added or modified.

### 7. Tests pass — PASS

```
yarn test:jest source/renderer/app/stores/MithrilPartialSyncStore.spec.ts
Tests: 25 passed, 25 total   Time: 0.557 s
```

All 4 new tests green. No pre-existing test regressions.

---

## Issues

None.

---

## Conclusion

The implementation is minimal and correct. It adds exactly the live-injection handoff coverage required by research-19 row #30, reuses the existing harness seam (`registeredStatusHandler`), avoids all enumerated duplicate paths, and all tests pass deterministically. Approve for commit and scribe.

Scribe: 2026-06-26 — verdict APPROVED, no changes required before commit.
