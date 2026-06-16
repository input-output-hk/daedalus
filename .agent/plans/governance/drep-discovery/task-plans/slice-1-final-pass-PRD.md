# Slice-1 Final Pass PRD

> **Status:** Draft | **Date:** 2026-06-12 | **Parent PRD:** [slice-1-PRD.md](./slice-1-PRD.md) | **Code Review:** [slice-1-code-review.md](./slice-1-code-review.md)

---

## Executive Summary

Slice-1 (Walking Skeleton + Sanitization Floor) has completed three implementation passes since its initial PRD. The core architecture is solid — real `cardano-cli` tuple parsing, CIP-129 DRep ID derivation, lossless `json-bigint` precision, and full sanitization coverage. However, a live-app smoke test against a preprod node revealed two defects that prevent the feature from functioning end-to-end, plus one UX polish item. This Final Pass PRD scopes the minimum work needed to close the remaining executable gaps before the slice can be considered fully verified.

### Scope Boundary

The Final Pass intentionally does NOT widen slice-1 scope. It fixes only what was discovered during live testing and cleans up documentation/bookkeeping artifacts. New features (cohort rules, DRep detail, favorites, etc.) remain deferred to their owning slices (5, 4, 7 respectively).

---

## Live-App Defects Discovered

### D1 (Blocker): CLI queries missing network flag

**Symptom:** Every `cardano-cli latest query drep-state` and `cardano-cli latest query tip` invocation fails with:

```
Missing: (--mainnet | --testnet-magic NATURAL)
```

**Root cause:** `GovernanceQueryService._runCliQuery()` sets `CARDANO_NODE_SOCKET_PATH` in the child process environment but never passes `--mainnet` or `--testnet-magic <N>` in the CLI argv. The installed `cardano-cli` in Daedalus's preprod environment requires an explicit network flag on every query.

**Impact:** The DRep directory never loads. All refresh/retry attempts fail identically. The service surface (selfnode guard, timeout, parser, error types) is unreachable behind this single missing flag.

**Fix approach:**
1. Add a `setNetwork(cluster: string)` method to `GovernanceQueryService` that derives `--mainnet` or `--testnet-magic <N>` from the cluster string passed by `CardanoNode`.
2. Pass the network flag in `_runCliQuery` args (or inject it in `_runCliQueryWithEraFallback` before the era flag).
3. Wire the call from `CardanoNode.start()` — the cardano-launcher branch already passes `cliBin` and socket path; add the cluster/network.

**Affected files:** `GovernanceQueryService.ts`, `CardanoNode.ts`

**Test requirement:** Add a Jest case that asserts the CLI args include the correct network flag for both mainnet (`--mainnet`) and testnet (`--testnet-magic`) clusters. Mock the cluster-to-flag mapping and verify argv contents.

---

### D2 (Major): Structured error details lost across IPC

**Symptom:** The renderer-side logger records `"error": {}` while the main-process logger records the full structured error with `queryErrorType` and `details`. The user sees only a generic "Could not load DRep data" message without the actionable CLI error text.

**Root cause (corrected — 2026-06-12 audit):** The error serialization chain has two weak links:

1. **Main → IPC (fragile but functional):** `governanceChannel.ts` throws `new Error(JSON.stringify({ type, message, details }))`. Electron's structured clone strips Error objects to plain `{ name: "Error", message: '{"type":...}' }`. The JSON payload survives inside `message`, and `GovernanceStore._normalizeError()` **already handles this case** — it attempts `JSON.parse(err.message)` before checking `instanceof Error` (see `GovernanceStore.ts:166-188`). The existing test "decodes JSON-wrapped IPC Error payloads correctly" confirms this path works. However, wrapping structured errors in `Error.message` is fragile — it depends on JSON surviving the structured clone round-trip inside a string field.

2. **UI rendering gap (the real defect):** The `DRepDirectoryPage.tsx` component renders `error.message` but NEVER renders `error.details`. So even when `_normalizeError` correctly extracts the CLI stderr text (e.g., `"Missing: --mainnet | --testnet-magic NATURAL"`), the user sees only the generic "Could not load DRep data" message. The actionable CLI error text is recovered from IPC but never displayed.

**Fix approach (two-part):**

**Part A — IPC transport hardening:** Replace the `new Error(JSON.stringify(...))` pattern in `governanceChannel.ts` with a plain object carrying a `__governanceError: true` marker. Plain objects survive structured clone without property loss (unlike Error instances which get flattened to `{ name, message }`). The `_normalizeError` method must be extended to check for this marker before the existing JSON.parse path.

**Part B — UI detail rendering (mandatory):** Extend `DRepDirectoryPage.tsx` to render `error.details` alongside `error.message` in both the error state and the error banner state. This is the critical fix — without it, the user never sees the actionable CLI error text regardless of how well the error is transported across IPC.

**Affected files:** `GovernanceStore.ts`, `governanceChannel.ts`, `DRepDirectory.tsx`

**Test requirement:** (1) Add a Jest case in `GovernanceStore.spec.ts` for the `__governanceError` marker path. (2) Keep the existing "decodes JSON-wrapped IPC Error payloads correctly" test for backward compatibility. (3) Add a Jest case in `DRepDirectory.spec.tsx` asserting that `error.details` appears in the rendered output when present.

---

### D3 (Minor): Duplicate hash history push on navigation

**Symptom:** Console warning on every click of the Governance sidebar tab or the Directory sub-tab:

```
Warning: Hash history cannot PUSH the same path; a new entry will not be added to the history stack
```

**Root cause:** Two separate code paths produce the same warning:
1. **Sub-tab clicks:** `Governance.tsx:32` — `this.props.history.push(itemId)` unconditionally pushes in `handleNavItemClick`, which handles Directory sub-tab clicks.
2. **Sidebar tab clicks:** The app-level sidebar navigation handler pushes `ROUTES.GOVERNANCE` on every Governance sidebar button click without checking the current route.

**Fix approach:**
1. Guard the sub-tab push in `Governance.tsx` with a current-path check:

```typescript
handleNavItemClick = (itemId: string) => {
  if (this.props.history.location.pathname !== itemId) {
    this.props.history.push(itemId);
  }
};
```

2. Locate and apply the same guard to the app-level sidebar navigation handler that pushes `ROUTES.GOVERNANCE`.

**Affected files:** `Governance.tsx`, sidebar navigation handler (to be identified during implementation)

**Test requirement:** The existing `DRepDirectory.spec.tsx` suite should be rerun to confirm no regression. A new Jest assertion should verify that `history.push` is NOT called when the current path already matches the target.

---

### D5 (Folded in from UX review): Query fires at app startup against an un-synced node

**Symptom:** `GovernanceStore.setup()` calls `fetchDRepList()` at app init, before the user visits Governance and before the node is synced / socket-ready. On a freshly launched preprod node this caches a `Failed` state and muddies the live smoke test (FP-8), because the directory's first observable state is an early failure rather than a clean route-entry query. `DRepDirectoryPage.componentDidMount` also fetches, so the startup call is also a double trigger.

**Root cause:** `source/main/`/renderer wiring fetches on store setup (`GovernanceStore.ts` `setup()` → `fetchDRepList()`) instead of only on Governance-route entry / explicit refresh.

**Decision (2026-06-15 UX review):** The DRep query must fire **only** on Governance-route entry (`DRepDirectoryPage.componentDidMount`) and explicit refresh — never from `GovernanceStore.setup()`. This is the UX-refinement "Query trigger" decision; it is folded into the final pass because it directly affects whether the FP-8 preprod smoke test reads cleanly. The remaining UX-refinement work (sync banner, two-phase load, per-phase timeouts, ID-only documentation) stays in the dedicated `ux-refinement` tracker phase.

**Fix approach:** Remove the `fetchDRepList()` call from `GovernanceStore.setup()`; keep the route-scoped trigger and the in-flight dedup in both the store and `GovernanceQueryService` so rapid route enters / refreshes coalesce.

**Affected files:** `GovernanceStore.ts`

**Test requirement:** Add a Jest case asserting no DRep query fires on store setup, and that route-entry / refresh still triggers a deduplicated fetch.

---

### D4 (Minor): DRep status term mismatch — `expired` vs ledger `inactive`

**Symptom:** The shipped `DRepStatus` union is `'active' | 'expired'`, where `expired` is derived from `expiry <= currentEpoch`. That condition is exactly what the Cardano ledger and the design tokens call **`inactive`** (a DRep that has not voted within its `drepActivity` window). The code term therefore contradicts [shared-design-tokens.md](../designs/shared-design-tokens.md) §1, the plan vocabulary, and the downstream slice-5 `Threshold` badge / cv-2 `CurrentVoteSummary` status badge — all of which are built on `active`/`inactive`/`expiring`.

**Decision (2026-06-15 review):** Keep the ledger-correct design vocabulary. The canonical stored status is `active | inactive`; `expiring` is a renderer-derived display state (remaining `drepActivity` ≤ 12 epochs); `retired` is deferred until a distinct unregistration signal is available. The code rename is tracked below as **FP-9**.

**Fix approach:** Rename the `DRepStatus` `'expired'` member to `'inactive'` in `source/common/types/governance.types.ts`, update the derivation in `GovernanceQueryService._parseDRepState` (`expiry <= currentEpoch ? 'inactive' : 'active'`), and update `DRepStatusBadge` + its i18n key (`status.inactive`) and any spec fixtures. No behavioural change — pure terminology alignment.

**Affected files:** `governance.types.ts`, `GovernanceQueryService.ts`, `DRepStatusBadge.tsx`/`.scss`, `DRepDirectory.messages` / locale catalogs, governance spec fixtures.

**Test requirement:** Update the existing `GovernanceQueryService.spec.ts` status assertions and `DRepStatusBadge` coverage to the `inactive` literal; no new test count target.

---

### E1 (Folded in from UX review): cardano-node 11 / LSM backend verification

**Context:** The flake pins `cardano-node` / `cardano-cli` to `11.0.1`. Node 11.0.1 introduces the **LSM-tree storage backend** (LedgerDB V2, replacing LMDB) for the UTxO-HD *OnDisk* backend. Before trusting preprod query results, confirm the DRep queries behave as the plan's mechanism model assumes.

**Assessment (from release notes / UTxO-HD docs):** UTxO-HD moves only the **UTxO set** to disk in this phase; `drep-state` and the DRep **stake distribution** are ledger-state components maintained incrementally and are served from in-memory ledger tables. So `drep-state --all-dreps --include-stake` is expected to be unaffected by the OnDisk/LSM backend, and the plan's "in-memory snapshot" framing holds. This is an assessment to **verify on the live node**, not an assumption to ship on.

**Verification checklist (FP-11):**
1. Confirm which UTxO-HD backend Daedalus's bundled 11.0.1 runs — InMemory (~24 GB RAM) vs OnDisk/LSM (~8 GB RAM). Record it; the "in-memory snapshot" framing is exact only for InMemory.
2. If OnDisk/LSM is used, confirm the new Linux runtime deps (`liburing`, `snappy-c`, `protobuf-compiler`) are present in the Nix closure.
3. Run `drep-state --all-dreps --include-stake` against the live preprod node (as part of FP-8) and confirm it returns and parses; capture the result as a real fixture candidate.
4. Note the known LSM bug — node cannot read blocks with >4096 items (blockio-uring, fix pending a later release) — as a sync-stall risk that would pin the directory in the soft-sync-banner state.

**Sources:** [cardano-node 11.0.1 release](https://github.com/IntersectMBO/cardano-node/releases/tag/11.0.1), [UTxO-HD overview](https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/utxo-hd/Overview/).

---

## Documentation & Bookkeeping

### B1: slice-1-code-review.md finalization (verified — already complete)

The `slice-1-code-review.md` has been incrementally updated across three implementation passes and is **already finalized**:
- Status is `finalized` in the document header.
- The Final Pass PRD is cross-referenced in the header.
- Immediate Next Steps section points to this PRD for remaining actionable gaps.

Verification confirmed 2026-06-12. No further action needed.

### B2: Evaluate the `origin` property in plan-tasks.json

The `origin` property in `governance-drep-discovery-plan-tasks.json` lists old task numbers from a prior tasking model (e.g., `"origin": ["task-001", "task-003"]`). These references are no longer useful:
- The old task numbering system has been completely replaced by the current `task-1XX` scheme.
- No tooling, script, or process reads the `origin` field.
- The original tasks they reference no longer exist in any document.

**Recommendation:** Remove the `origin` property from all task entries. This is a mechanical cleanup — no logic depends on it.

Additional stale references to evaluate:
- Task descriptions that reference "dissolved" tasks and old hour splits (e.g., `"(Split from task-008: bare-list portion, ~4 of task-008's 8h plus dissolved Storybook/Jest coverage hours)"`). These parentheticals are implementation-history notes that can be removed or consolidated into the `origin` field if kept.

### B3: Task status verification in plan-tasks.json

**Confirmed (2026-06-12 audit):** Task statuses in `governance-drep-discovery-plan-tasks.json` are already correct:
- **task-102** (`complete`): Selfnode guard, typed `SelfnodeCliUnsupported`, socket wiring, and reset cleanup all landed.
- **task-104** (`complete`): IPC `details` field preserved through JSON serialization; `_normalizeError` already decodes it.
- **task-107** (`complete`): Pagination coverage, ja-JP locale rendering, and all Jest assertions landed.

Only **task-103** (query service) remains `partial` due to the `active`/`expired` semantics gap (real CLI only provides `expiry`, not a richer status signal).

**Action:** Verify the `auditSummary.statusCounts` reflect the actual state (`complete: 7, partial: 1, verified: 3`). The sprint task FP-5 is reduced to verification-only — no status changes needed.

---

## Final Pass Task Breakdown

| ID | Task | Priority | Est. Hours |
|---|---|---|---|
| FP-1 | Add network flag to CLI queries (`--mainnet` / `--testnet-magic`) | Blocker | 3 |
| FP-2 | Fix structured error loss across IPC → Store | Major | 3 |
| FP-3 | Guard duplicate hash history push (sidebar + sub-tab) | Minor | 0.5 |
| FP-4 | Verify slice-1-code-review.md finalization (already complete) | Medium | 0.5 |
| FP-5 | Verify task statuses in plan-tasks.json | Medium | 0.5 |
| FP-6 | Evaluate and clean `origin` property + stale task references in plan-tasks.json | Low | 1 |
| FP-7 | Rerun full focused governance test suite (all 35+ tests) | Blocker | 0.5 |
| FP-8 | Rerun live-app smoke test against preprod node post-fix | Blocker | 1 |
| FP-9 | Rename `DRepStatus` `'expired'` → `'inactive'` (ledger/design vocab alignment, see D4) | Minor | 1 |
| FP-10 | Remove `GovernanceStore.setup()` auto-fetch; query fires on route entry + refresh only (see D5) | Major | 1 |
| FP-11 | Verify cardano-node 11 / LSM backend behaviour for the DRep queries during the preprod smoke test (see E1) | Medium | 1 |

**Total estimated effort: ~13 hours**

---

## Acceptance Criteria (Final Pass)

- [ ] `cardano-cli` queries succeed on both mainnet and preprod/testnet clusters
- [ ] CLI error text (e.g., "Missing: --mainnet | --testnet-magic") is visible in the renderer error UI
- [ ] Refresh and Retry buttons produce actionable error messages
- [ ] No "Hash history cannot PUSH the same path" warnings in console
- [ ] `GovernanceStore._normalizeError` handles both Error instances and plain objects from IPC
- [ ] `slice-1-code-review.md` is finalized with a pointer to this PRD
- [ ] Task-102, 104, 107 statuses are accurate in plan-tasks.json
- [ ] `origin` property evaluation is complete (removed or justified)
- [ ] `DRepStatus` uses the ledger-correct `active`/`inactive` vocabulary (FP-9); `expiring`/`retired` remain deferred per design tokens
- [ ] No DRep query fires from `GovernanceStore.setup()`; the query fires on Governance-route entry and explicit refresh only, deduplicated (FP-10)
- [ ] The cardano-node 11 / LSM backend behaviour for `drep-state --all-dreps --include-stake` is verified on the live preprod node and recorded (FP-11)
- [ ] Focused governance test suite passes (35+ tests, 0 skipped)
- [ ] `yarn compile` passes with zero TS errors

---

## Out of Scope (remaining post-final-pass gaps)

- **Real synced-node fixtures** — committed mocks remain synthetic; real-node capture is deferred to slice-2 pre-work or a standalone fixture-refresh task.
- **Repository-wide lint/prettier** — non-governance baseline failures remain; governance files pass focused checks.
- **Slice-2 PRD and implementation guide** — not yet created; all slice-2 tasks remain `pending`.
