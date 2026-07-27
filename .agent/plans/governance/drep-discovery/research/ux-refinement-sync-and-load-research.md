# Research — Sync Awareness, First-Load Latency & Real-Data Validation

> **Status:** Research (decisions ratified — see "Where this is now tracked") | **Date:** 2026-06-15 | **Parent plan:** [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md) | **Designs:** [drep-discovery-design.md](../designs/drep-discovery-design.md), [shared-design-tokens.md](../designs/shared-design-tokens.md)
>
> **Author:** david-profrontsolutions (ft. Claude)

> **Reframed from a draft PRD into research on 2026-06-15.** The four decisions below have been folded into the canonical planning documents; this file is retained as the research record (mechanism analysis, as-built gaps, authoritative basis, and the cardano-node 11 / LSM findings) behind those decisions. It is no longer the tracking surface — see the next section.

---

## Where this is now tracked

| Decision / work | Canonical home |
|---|---|
| The four decisions (soft-sync banner, two-phase load, per-phase timeouts, ID-only v1) | [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md) **Key Decisions** (`Sync behaviour`, `First-load shape`, `Query trigger`, `CLI timeouts`, `Directory names (v1)`) |
| Two-phase + soft-sync state model | [shared-design-tokens.md](../designs/shared-design-tokens.md) §6 and [drep-discovery-design.md](../designs/drep-discovery-design.md) state table |
| Stale-list and unvalidated-timeout risks + deferred latency follow-up | plan **Risks And Mitigations** |
| Implementation tasks (former UX-1, UX-2, UX-4…UX-10) | `ux-refinement` phase in [governance-drep-discovery-plan-tasks.json](../governance-drep-discovery-plan-tasks.json) (task-159…task-167) |
| Route-scoped fetch trigger (former UX-3) | slice-1 final pass **FP-10** ([slice-1-final-pass-PRD.md](../task-plans/slice-1-final-pass-PRD.md)) — folded in because it affects the FP-8 preprod smoke test |
| cardano-node 11 / LSM verification | slice-1 final pass **FP-11** (see [cardano-node 11 / LSM Findings](#cardano-node-11--lsm-findings) below) |

The Requirements, Task Breakdown, Affected Files, and Acceptance Criteria sections below are preserved as the original research record; the authoritative, trackable versions live in the homes above.

---

## Executive Summary

A review of the DRep Discovery plans against the as-built slice-1 code and authoritative Cardano sources surfaced a cluster of **sync-awareness and first-load gaps** that the existing plans specify on paper but do not implement, plus one **mechanism misconception** worth recording so future contributors do not "fix" the wrong thing.

This PRD scopes four refinements:

1. **Sync-aware directory** — surface node-sync state in the directory instead of silently serving an incomplete list while the node is behind the chain tip.
2. **Two-phase first load** — paint the DRep list from the cheap registration read first, then enrich voting power, so the expensive `--include-stake` computation no longer blocks first paint.
3. **Defensive timeout + deferred real-data validation** — raise the CLI timeout and explicitly record that the query has never run against mainnet-scale data.
4. **ID-only directory for v1 (documented limitation)** — confirm and document that directory cards and search are DRep-ID-only until a future bulk anchor-prefetch phase.

This PRD does **not** widen feature scope (no cohort/detail/favorites/anchor work is added). It hardens the *load and sync experience* of the surfaces already planned.

---

## Mechanism Correction (record this — it is the root of the gaps)

A natural assumption is that the DRep directory will populate slowly and progressively, like a newly-added wallet's transaction history (which shows a 0–100% restoration bar). **This is not how DRep data loads, and the two must not be conflated.**

| | Wallet transaction history | DRep directory |
|---|---|---|
| Source | `cardano-wallet` **scans every block** for the wallet's addresses ("restoration") | `cardano-cli query drep-state --all-dreps` reads the node's **in-memory current ledger state** |
| Cost driver | Per-address historical replay of the whole chain | None historical — a single local-state-query snapshot |
| Delivery | Incremental; per-wallet `syncState.progress.quantity` 0–100% | **One blocking call, whole JSON at once. No streaming, no progressive populate.** |
| Freshness | Builds up over time | Complete as of the node's *current local tip* the instant it returns |

**Consequences that drive this PRD:**

- There is **no "parse the DB for DReps" phase** and **no per-epoch DRep discovery** to optimize. A wallet-style percentage bar for the DRep list is neither needed nor possible.
- The *only* relevant wait is the **global node sync** (shared with all of Daedalus). Until the node reaches tip, `drep-state` returns a **correct-but-stale snapshot as of the node's local tip** — internally consistent, but missing DReps registered after that point. It does not error (except era-mismatch before Conway) and does not block. → This is exactly why the directory must be sync-aware.
- DRep **anchor metadata is not in ledger state** — only the `(url, blake2b-hash)` anchor pair is. Names/descriptions require a separate per-DRep HTTP fetch (anchor-1+). The plan correctly defers this; the cost is an ID-only directory for v1.

**Authoritative basis:** local-state-query is a request/response snapshot protocol (ogmios.dev/mini-protocols/local-state-query); the DRep stake distribution is *maintained in ledger state* (cardano-ledger #3446); `--include-stake` is documented as a *"potentially expensive query, so it's OFF by default"* (cardano-cli source); anchor = URL + hash only, content off-chain and unchecked by the ledger (CIP-1694). No published mainnet latency benchmark for `drep-state --all-dreps --include-stake` exists — measurement against a synced node is the only authority.

---

## cardano-node 11 / LSM Findings

Added 2026-06-15 to confirm the mechanism model holds on the pinned node. The flake pins `cardano-node` / `cardano-cli` to **11.0.1**, the first release shipping the **LSM-tree storage backend** (LedgerDB V2, replacing LMDB) for the UTxO-HD *OnDisk* backend.

**Verdict — the DRep queries are expected to be unaffected.** UTxO-HD moves only the **UTxO set** to disk in this phase ("starting with moving the UTXO set on-disk… *finally* identifying other ledger state components to migrate"). `drep-state` and the DRep **stake distribution** are ledger-state components maintained incrementally (cardano-ledger #3446), **not** the UTxO set, so `drep-state --all-dreps --include-stake` is served from in-memory ledger tables. The "in-memory snapshot" mechanism model above holds on node 11. The disk-trip performance regression the docs warn about applies to operations that read the on-disk UTxO set — which these queries do not.

**Caveats to verify on the live node (tracked as final-pass FP-11):**

- **Active backend.** Confirm whether the bundled 11.0.1 runs InMemory (~24 GB RAM; the "in-memory snapshot" framing is then exact) or OnDisk/LSM (~8 GB RAM). Record it.
- **New Linux deps.** If OnDisk/LSM is active, `liburing`, `snappy-c`, and `protobuf-compiler` are new runtime deps since 10.7 and must be present in the Nix closure.
- **Known LSM bug.** Node 11.0.1 with the LSM backend cannot read blocks with **>4096 items** (blockio-uring; fix pending a later release). A stalled sync from this would pin the directory in the soft-sync-banner state — relevant to preprod testing.

**Considered and dropped:** the 2026-07-27 cross-slice planning sweep weighed a task-125 amendment carrying these caveats forward and left it below the cutoff. FP-11 did run — [slice-1-final-pass-review.md](../task-plans/slice-1-final-pass-review.md) records `cardano-cli 11.0.0.0`, `cardano-node 11.0.1` and `LedgerDB.Backend: "V2InMemory"` — but it ran against `Daedalus 11.0.0#dev`, while `nix/internal/launcher-config.nix:295` forces `Backend: "V2LSM"` on packaged builds. The reading is therefore the dev-shell backend, and the three caveats above stay unverified for a release build. The named absorption — recording `cardano-cli --version`, `cardano-node --version` and the effective `LedgerDB.Backend` during the task-125 release-verification session — was considered and dropped with it: task-125 keeps its single manual release-verification criterion and gained no provenance criterion, on the same reasoning that cut the sibling task-125 riders (see `slice-2-findings.md` Storybook bullet and `slice-4-findings.md` F-1). The residual is bounded and is this paragraph: no latency figure measured in a dev shell may be used to size a production CLI timeout, because the packaged build runs a different LedgerDB backend than the one FP-11 measured. The zero-vs-null voting-power item merged into the same below-cutoff entry is recorded here rather than in its own findings file, which is closed precedent: all six touch points are correct as shipped — `BigNumber(0)` is truthy, so `DRepCard.tsx:69`'s `if (!value) return '—'` does not swallow a real zero-stake DRep, which renders `₳ 0` distinctly from `—` — the failure story needs a future edit nobody has proposed, and no Jest pin was added to task-172 or to any other host.

**Sources:** [cardano-node 11.0.1 release notes](https://github.com/IntersectMBO/cardano-node/releases/tag/11.0.1); [UTxO-HD high-level overview](https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/utxo-hd/Overview/). No published mainnet latency benchmark for `drep-state --all-dreps --include-stake` on node 11 exists — measurement against a synced node remains the only authority (the deferred follow-up).

---

## Gaps Confirmed in the As-Built Code

| # | Gap | Evidence |
|---|---|---|
| G1 | **Directory never consults node-sync state.** `GovernanceStore` does not import `NetworkStatusStore`; no `isNodeInSync` / `syncProgress` check before querying. The designed `noSync` state ([shared-design-tokens.md §6 `empty.noSync`], [drep-discovery-design.md state table]) is **never wired**. | `GovernanceStore.ts` (no NetworkStatus import); `DRepDirectory.tsx` has no syncing branch. |
| G2 | **Query fires at app startup**, before the user visits Governance and possibly before the node is synced/socket ready, caching a `Failed` state. | `GovernanceStore.setup()` calls `fetchDRepList()` at init ([GovernanceStore.ts:148]); `DRepDirectoryPage.componentDidMount` also fetches ([DRepDirectoryPage.tsx:16]) — double trigger. |
| G3 | **`--include-stake` (expensive) is on the first-paint path** as a single blocking call. | `GovernanceQueryService._doFetchDRepList` runs `drep-state --all-dreps --include-stake` + `query tip` together ([GovernanceQueryService.ts:151-163]). |
| G4 | **10s timeout is an unvalidated guess; query never run on mainnet-scale data.** | `CLI_TIMEOUT_MS = 10_000` ([GovernanceQueryService.ts:51]); plan admits "Live DRep JSON fixture capture could not be completed" and final-pass out-of-scope: "committed mocks remain synthetic." |
| G5 | **Directory cards are ID-only** (no names; ID-only search) and this is not stated as an explicit, accepted v1 limitation in user-facing planning. | Anchors fetched lazily per detail visit (anchor-1); name search deferred ([shared-design-tokens.md §11]). |

---

## Decisions (this PRD)

| Area | Decision | Rationale |
|---|---|---|
| **Sync behaviour** | **Soft warning, query anyway.** Render the directory and run the query during sync, with a persistent, sync-percentage-driven banner that the list may be incomplete until 100%. | Lets users browse early; avoids a hard wall. The existing `noSync` empty state is retained only as the fallback when the query yields no data or errors during sync. |
| **First load** | **Two-phase load.** Phase 1: `drep-state --all-dreps` *without* `--include-stake` → paint the list immediately. Phase 2: enrich voting power via `drep-stake-distribution --all-dreps` (or `--include-stake`) under stale-while-refresh; voting-power column shows `—`/skeleton until it lands. | Decouples cheap registration read from expensive stake computation; reuses the already-designed `rankingUnavailable` partial state ([shared-design-tokens.md §6]). |
| **Timeout / validation** | **Raise the stake-phase timeout to 30s now**, keep the bare-list phase at 10s. Real synced-node fixtures + measured latency remain a deferred follow-up (not blocking this PRD). | Defensive against a slow first mainnet load without waiting on a measurement task; the number stays explicitly "a guess pending measurement." |
| **Names in directory** | **Accept ID-only directory + ID-only search for v1.** Document the limitation; verified names appear in the detail view (anchor-1) and confirmation only. | Keeps v1 scope tight and performance safe; bulk anchor prefetch (N hardened HTTP fetches) is a separate future phase. |

---

## Requirements

### R1 — Sync-aware directory (soft warning)

- [ ] `DRepDirectoryPage` (container) reads `stores.networkStatus.isNodeInSync` and `stores.networkStatus.syncProgress` and passes them into `DRepDirectory` as props. `GovernanceStore` itself stays decoupled from `NetworkStatusStore` (keep the store boundary; the container is the integration point).
- [ ] When `!isNodeInSync`, `DRepDirectory` renders a persistent, non-dismissible **syncing banner** above the list:
      `governance.drepDirectory.syncing` → *"!!!Your node is still syncing ({progress}%). The DRep list may be incomplete until sync completes."*
- [ ] The banner uses the existing inline-warning visual pattern (shared with voting screens) and the `--badge-warning-*` token slots from [shared-design-tokens.md §1]. Color is never the sole cue (icon + text).
- [ ] If, while syncing, the query returns **zero DReps** or fails with an era/availability error, the list area falls back to the existing `DRepEmptyState noSync` (`governance.drepDirectory.empty.noSync`) instead of a bare error. A synced node that genuinely has data shows the data with no banner.
- [ ] When `isNodeInSync` flips to true, the banner clears and a fresh fetch runs (or the existing stale-while-refresh path is triggered) so the user sees the complete list without a manual refresh.

### R2 — Single, route-scoped fetch trigger (fixes double/early fire)

- [ ] Remove the auto-fetch from `GovernanceStore.setup()`; the query must fire only when the user is on the Governance directory route (`DRepDirectoryPage.componentDidMount`) or on explicit refresh. This stops the app-startup query against an un-synced node and the double trigger (G2).
- [ ] Keep the in-flight dedup in both the store and `GovernanceQueryService` so rapid route enters / refreshes coalesce.

### R3 — Two-phase load

- [ ] Split `GovernanceQueryService.fetchDRepList()` into:
  - **Phase 1 — `fetchDRepRegistrations()`**: `cardano-cli latest query drep-state --all-dreps --output-json` (no `--include-stake`) + `query tip`. Returns the directory entries with `votingPower: null`. Timeout **10s**.
  - **Phase 2 — `fetchDRepStake()`**: `cardano-cli latest query drep-stake-distribution --all-dreps --output-json`. Returns a credential→lovelace map merged into the existing entries by DRep credential. Timeout **30s**.
- [ ] `GovernanceStore` drives the two phases: phase 1 → `Loaded` (list visible, voting-power column `—` with the existing "Stake distribution unavailable this refresh"/loading tooltip); phase 2 success → voting power populated; phase 2 failure → keep the list, surface the already-designed `error.rankingUnavailable` banner and leave `—`.
- [ ] Preserve all existing guarantees: lossless `json-bigint` parse, decimal-string IPC, `BigNumber` rehydrate, network-flag + era-fallback on **both** CLI calls, `CARDANO_NODE_SOCKET_PATH` via `spawn.env` only.
- [ ] Manual **Refresh** re-runs both phases. **Reshuffle** still reseeds without re-querying (unchanged).

### R4 — Defensive timeout + recorded validation debt

- [ ] Bare-list phase timeout = `10_000` ms; stake phase timeout = `30_000` ms (replace the single `CLI_TIMEOUT_MS` with per-phase budgets).
- [ ] Update the [shared-design-tokens.md §6] refresh-state time budgets to match the two-phase model (skeleton ≤700ms; list ≤10s; voting-power enrich ≤30s; `rankingUnavailable` banner at 30s).
- [ ] Add an explicit **deferred follow-up** note (here and in the plan's Rollout/Risks): *capture real synced-node fixtures (mainnet + preprod), record p50/p95 latency for both phases, and re-derive the 30s budget from data.* This remains out of scope for the PRD's shippable work but must not be lost.

### R5 — Documented ID-only v1 limitation

- [ ] Record in the plan and the directory design that **v1 directory cards and search are DRep-ID-only**; verified `givenName` appears only in the detail view (anchor-1) and confirmation, and name search is deferred to a bulk anchor-prefetch phase.
- [ ] The ID-only directory must remain fully usable: dual-ID display, copy, status badge, voting power (post phase-2), and the View details / Select CTAs — no name dependency anywhere on the card.

---

## Affected Files

| Concern | Files |
|---|---|
| Two-phase query | `source/main/governance/GovernanceQueryService.ts` |
| Sync props + single trigger | `source/renderer/app/containers/governance/DRepDirectoryPage.tsx`, `source/renderer/app/stores/GovernanceStore.ts` |
| Syncing banner + ranking-unavailable wiring | `source/renderer/app/components/governance/drep-directory/DRepDirectory.tsx`, `DRepDirectoryBanner.tsx`, `_shared/` |
| i18n | `source/renderer/app/i18n/locales/governance/*` (+ `en-US.json` / `ja-JP.json` via `yarn i18n:manage`, keep `!!!`) |
| Design docs | [shared-design-tokens.md](../designs/shared-design-tokens.md) §6, [drep-discovery-design.md](../designs/drep-discovery-design.md) state table, [governance-drep-discovery-plan.md](../governance-drep-discovery-plan.md) Key Decisions / Risks / Changelog |

---

## Task Breakdown

> **Original research breakdown (now tracked elsewhere).** UX-1, UX-2, UX-4…UX-10 are tracked as `task-159`…`task-167` in the `ux-refinement` phase of the companion tracker. **UX-3 moved to the slice-1 final pass as FP-10** (it affects the FP-8 preprod smoke test). The `Tracked as` column records the mapping.

| ID | Task | Priority | Est. | Tracked as |
|---|---|---|---|---|
| UX-1 | Container reads `networkStatus.isNodeInSync` + `syncProgress`; pass to `DRepDirectory` | High | 1.5h | task-159 |
| UX-2 | Persistent syncing banner + `noSync` fallback wiring + clear-on-sync refetch | High | 2h | task-160 |
| UX-3 | Remove `GovernanceStore.setup()` auto-fetch; single route-scoped trigger; keep dedup | High | 1h | **FP-10 (final pass)** |
| UX-4 | Split query service into registrations + stake phases; merge stake by credential | High | 3h | task-161 |
| UX-5 | Store drives two phases → `Loaded` then enrich; `rankingUnavailable` on stake failure | High | 2.5h | task-162 |
| UX-6 | Per-phase timeouts (10s / 30s); update §6 time budgets | Medium | 0.5h | task-163 |
| UX-7 | i18n: `syncing` key + ja-JP placeholder; voting-power loading tooltip copy | Medium | 1h | task-164 |
| UX-8 | Document ID-only v1 limitation in plan + design | Low | 0.5h | task-165 |
| UX-9 | Record deferred real-fixture + latency-measurement follow-up | Low | 0.5h | task-166 |
| UX-10 | Jest: sync-banner render, two-phase store transitions, stake-failure → rankingUnavailable (no-startup-fetch test now lands with FP-10) | High | 3h | task-167 |

**Total ≈ 15.5h** (UX-3 ≈ 1h of this is now FP-10 in the final pass)

---

## Acceptance Criteria

- [ ] On an un-synced node, the directory shows the syncing banner with the live sync %, runs the query, and shows whatever data is available; zero-result/era failures fall back to `noSync` (never a bare error).
- [ ] When the node reaches tip, the banner clears and the full list appears without a manual refresh.
- [ ] No DRep query fires at app startup; the query fires on Governance-route entry and explicit refresh only, deduplicated.
- [ ] First paint shows the DRep list from the bare registration read; voting power fills in afterward (or shows `—` + `rankingUnavailable` if the stake phase fails).
- [ ] Bare-list phase uses a 10s timeout; stake phase uses 30s; both carry the network flag and era fallback.
- [ ] Directory cards and search work with DRep IDs only; no card renders a name in v1; the limitation is documented.
- [ ] `shared-design-tokens.md` §6 and `drep-discovery-design.md` state table reflect the two-phase + soft-sync model; the plan changelog records all four decisions.
- [ ] `yarn compile`, focused governance Jest suite, and i18n manage pass.

---

## Out of Scope / Deferred

- **Real synced-node fixtures & measured latency** (mainnet + preprod) — deferred follow-up; the 30s budget is provisional until measured.
- **Bulk anchor prefetch / names in directory / name search** — future phase; v1 stays ID-only.
- **Hard-gating or blocking the Governance nav during sync** — rejected in favor of the soft-warning approach.
- Any cohort, detail, favorites, or anchor-pipeline feature work — owned by their existing slices.
