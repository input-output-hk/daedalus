# Slice-1 DRep Discovery — Code Review

> **Branch:** `feat/drep-discovery` | **Date:** `2026-06-12` | **Status:** `finalized` | **PRD:** [slice-1-PRD.md](./slice-1-PRD.md) | **Final Pass PRD:** [slice-1-final-pass-PRD.md](./slice-1-final-pass-PRD.md) | **Guide:** [slice-1-implementation-guide.md](./slice-1-implementation-guide.md)

> **2026-06-12 residual completion pass.** The remaining slice-1 residuals have been resolved: fail-fast parsing, selfnode guard, CLI subprocess timeout, IPC `details` preservation, pagination-focused Storybook/Jest coverage, and ja-JP Storybook variant.

> **2026-06-12 parser/schema repair pass.** The `GovernanceQueryService` parser has been replaced with a real tuple parser that consumes the actual CLI output shape of `cardano-cli latest query drep-state --all-dreps --include-stake --output-json`. CIP-129 bech32 DRep IDs are now derived from on-chain credentials using `Cardano.DRepID.cip129FromCredential` from `@cardano-sdk/core`. Status is derived from `expiry` vs current epoch fetched via `query tip`, and the query now falls back from `latest` to `conway` if the installed CLI does not expose the `latest` alias. Voting power is nullable when `--include-stake` returns no `stake` field. The singleton service now exposes a `reset()` method used by `CardanoNode._reset()`. Mock fixtures now match the real tuple and map shapes. Focused Jest now passes for the repaired query service, renderer store precision regression, and directory slice with no skipped tests.

> **2026-06-12 live-app smoke test.** Three defects were discovered against a preprod node: CLI queries are missing the network flag (`--mainnet` / `--testnet-magic`), structured error details are lost across IPC, and the Governance tab emits duplicate hash history warnings. These are scoped and tracked in the [Final Pass PRD](./slice-1-final-pass-PRD.md), not in this review document. This code review is now finalized — remaining executable gaps are owned by the Final Pass PRD.

---

## Overview

This review reflects the post-repair workspace. The main-process governance service uses real CLI output shapes and derives CIP-129 DRep IDs from on-chain credentials. Status determination is conservative (only `active`/`expired` from `expiry` vs `currentEpoch`). Voting power is nullable at the shared type boundary to match the optional `stake` in the CLI output.

Focused validation completed in this pass:

1. `yarn compile` — zero TS errors
2. `yarn test:jest tests/jest/governance/GovernanceQueryService.spec.ts --runInBand` (23 tests passing)
3. `yarn test:jest tests/jest/governance/GovernanceStore.spec.ts --runInBand` (3 tests passing)
4. `yarn test:jest source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx --runInBand` (9 tests passing)
5. `yarn test:jest tests/jest/governance/GovernanceQueryService.spec.ts tests/jest/governance/GovernanceStore.spec.ts source/renderer/app/components/governance/drep-directory/DRepDirectory.spec.tsx --runInBand` (35 tests passing)
6. Governance component-cluster ESLint passes for the touched governance UI files.
7. Touched governance files pass `prettier --check`.

### Parser repair changes in this pass:

1. **Rewrote `_parseDRepState`** to handle the real `[[credential, state], ...]` tuple output from `cardano-cli query drep-state --all-dreps --include-stake --output-json`.
2. **Added `query tip` call** to derive current epoch for status/activity calculation.
3. **Added `--include-stake`** to the CLI invocation so voting power is available when the node provides it.
4. **Derive CIP-129 DRep IDs** from credentials using `Cardano.DRepID.cip129FromCredential` — no hand-rolled bech32 encoding.
5. **Era compatibility fallback** — `latest` is still preferred, but the query now retries with `conway` when the installed CLI rejects the `latest` era alias.
6. **Strict tip parsing** — `query tip` is now required to produce an integer epoch scalar; malformed or coercible non-scalar tip output fails the refresh instead of defaulting DReps to `active`.
7. **Conservative status** — only `active`/`expired` from `expiry` vs `currentEpoch`. The original `inactive`/`retired` statuses are removed because they cannot be grounded from `drep-state` tuple output alone.
8. **Strict anchor parsing** — partial anchors or non-string anchor fields now fail parsing instead of producing broken `{ url: '', hash: '...' }` placeholder data.
9. **Nullable voting power** — `DRepDirectoryEntry.votingPower` is now `Lovelace | null` to match the optional `stake` field.
10. **Added `reset()` method** to `GovernanceQueryService` — clears `lastSuccessfulData`, `inFlightRefresh`, `nodeSocketPath`, and `isSelfnode`. Called from `CardanoNode._reset()`.
11. **Updated mock fixtures** — `drep-state.json` now uses real tuple shape; `drep-stake-distribution.json` uses real map/object shape.
12. **Rewrote `GovernanceQueryService.spec.ts`** — 23 comprehensive tests using deterministic spawn mocks: successful tuple parsing, `conway` fallback, raw oversized-lovelace preservation through `json-bigint`, strict tip parsing, parse failures (non-array, malformed tuple, unknown credential, missing expiry, coercible non-scalar expiry/tip values, partial anchor, invalid anchor types, invalid JSON), timeout, CliNotFound, cache reset, dedup.
13. **Added `GovernanceStore.spec.ts`** — verifies renderer-side `BigNumber` rehydration preserves the oversized decimal-string lovelace value exactly, and covers both plain-object and JSON-wrapped IPC error normalization paths.
14. **Updated `DRepStatusBadge`** and related components to match the reduced status union, including removal of stale `inactive` / `retired` catalog and SCSS artifacts.
15. **Expanded Storybook coverage** with Loading in en-US plus Empty/Error/Refreshing ja-JP variants and pagination variants for both locales.
16. **Added stronger pagination and loading assertions** in `DRepDirectory.spec.tsx` — page navigation, entries-per-page count check, locale rendering, and the loading-state path all execute in Jest.

### Remaining partial/open items:

- **`active`/`inactive` semantics**. The original plan assumed a richer `status` signal than the real CLI provides. The repaired parser grounds only the two states derivable from `expiry` vs current epoch. **Resolved (2026-06-15 review):** the canonical vocabulary stays ledger-correct — the condition the code currently labels `expired` is the ledger's `inactive`, so `DRepStatus` is being renamed `expired` → `inactive` (tracked as final-pass FP-9). `Expiring` is a renderer-derived display state and `Retired` is deferred until a distinct unregistration signal exists. See [Final Pass PRD](./slice-1-final-pass-PRD.md) D4.
- **Live renderer pass against a synced node**. The real IPC path has not yet been exercised against a running node after the parser repair.
- **Real synced-node fixtures**. The committed governance fixtures remain synthetic examples rather than captured outputs from a real synced node.
- **Repository-wide `yarn lint` and `yarn prettier:check`** remain blocked by pre-existing baseline issues outside the governance slice. The touched governance files now pass focused ESLint / Prettier checks.
- **P2.4, P2.5, P2.6** remain open by inheritance.

---

## Findings

### R5 — Parser/schema repair (2026-06-12)

The original parser assumed a flat-object CLI output with `drepId`, `votingPower`, `status`, and `drepActivity` fields — none of which exist in the real `cardano-cli query drep-state --output-json` output. This pass replaced the entire parser with one that reads the real tuple shape, derives CIP-129 DRep IDs from credentials, and computes status/activity from `expiry` vs current epoch. See the overview for full details.

### ✅ R1–R4 — Resolved in 2026-06-12 Residual Completion Pass

All four residual findings (R1: fail-fast parsing, R2: service stale fallback + IPC error transport, R3: selfnode/timeout typing, R4: task-107 coverage) have been resolved in the prior pass and remain intact after the repair.

### Historical note

The prior blocker findings are now closed in code and retained only as historical context: runtime store instantiation, CardanoNode governance-service wiring/reset, the `react-intl@2.9.0` migration, sidebar rename, directory stories, locale catalogs, governance fixtures, analytics schema cleanup, logger/analytics spy coverage, localized refreshing text, clipboard failure handling, and recursive array-safe redaction are all landed in the current workspace.

---

## Task-By-Task Status

| Task | Status | Notes |
|------|--------|-------|
| task-101 (types) | `complete` | Shared types updated: `votingPower` is now `Lovelace \| null`, `DRepStatus` reduced to `'active' \| 'expired'`. Compiles cleanly. |
| task-102 (socket path) | `complete` | Socket ownership, service wiring, reset cleanup, and explicit selfnode guard are landed. `CardanoNode._reset()` now calls `GovernanceQueryService.reset()`. |
| task-103 (query service) | `partial` | Fixtures, socket wiring, `json-bigint` parsing, `latest` -> `conway` fallback, in-flight dedupe, fail-fast parsing, strict tip/anchor parsing, 10 s CLI timeout, selfnode/Timeout typing, and CIP-129 derivation from credentials are all landed. **Partial**: (1) the CLI invocation is still missing the network flag — fixed in FP-1; (2) the status literal `expired` is the ledger's `inactive` and is being renamed in FP-9. The real CLI output grounds only the two `expiry`-vs-epoch states; `expiring` (derived) and `retired` (needs a separate signal) remain deferred. See R5. |
| task-104 (IPC channels) | `complete` | DRep list IPC path works end-to-end; `details` field preserved through IPC JSON serialization. |
| task-105 (renderer IPC) | `complete` | Renderer governance channel wrapper exists and compiles with the current slice state. |
| task-106 (GovernanceStore) | `complete` | The store is instantiated at runtime and the page retries on first navigation after idle/failed bootstrap. |
| task-107 (directory components) | `complete` | Directory renders, i18n/catalog entries landed, stories include en-US Loading plus ja-JP Loaded/Empty/Error/Refreshing and pagination variants for both locales. Jest covers loading, empty, blocking error, retained-list error, pagination controls, navigation, entries-per-page, and ja-JP locale rendering. |
| task-108 (route wiring) | `complete` | Governance routes, sidebar rename, and active-state handling for both `/governance/*` and `/voting/*` are landed. |
| task-109 (log redaction) | `verified` | Recursive governance-key redaction and logger-boundary leakage assertions are covered by focused Jest tests. |
| task-110 (analytics) | `verified` | The analytics event now emits only `drepOption`, the numeric `value` slot is gone, and the CHANGELOG note is present. |
| task-111 (spy floor) | `verified` | Focused Jest coverage now spies on both logger and analytics boundaries to enforce the inherited sanitization floor. |

---

## Definition Of Done Cross-Check

| DoD Item | Status | Notes |
|----------|--------|-------|
| `yarn compile` passes with zero TS errors | `verified` | Run in this pass (2026-06-12) — passes after parser repair. |
| `yarn lint` passes | `partial` | Governance component-cluster ESLint passes. Repository-wide `yarn lint` still fails on unrelated baseline issues outside this slice. |
| `yarn prettier:check` passes | `partial` | Touched governance files pass `prettier --check`. Repository-wide `yarn prettier:check` still reports unrelated baseline issues outside this slice. |
| DRep directory renders on `/governance/dreps` with real IPC data | `unverified` | Code path is in place, but no synced-node runtime pass was executed. |
| Loading, empty, and error states are reachable | `verified` | Loading, empty, blocking error, and stale-retained error paths are covered by focused Jest and Storybook. |
| Voting power displays in `BigNumber`-rehydrated ADA with no precision loss | `partial` | Oversized unquoted lovelace parsing is covered in the governance service tests and renderer-side `BigNumber` rehydration is covered in `GovernanceStore.spec.ts`; a live synced-node end-to-end pass is still unverified. |
| `filterLogData` redacts governance vote keys | `verified` | Covered by the focused governance sanitization suite. |
| `'Casted governance vote'` analytics carries `drepOption` only | `verified` | Covered by focused Jest assertions and the implementation change. |
| Jest spy test confirms zero leakage to logs/analytics | `verified` | Covered by focused logger + analytics spy tests. |
| Dedicated Storybook stories exist for directory states | `complete` | Stories exist for all core states. en-US covers `Loaded`, `Empty`, `Error`, `Loading`, `Refreshing`, plus pagination; ja-JP covers `Loaded`, `Empty`, `Error`, `Refreshing`, plus pagination. |
| i18n keys exist for all user-visible governance text | `verified` | `i18n:manage` was run and the locale catalogs were updated with temporary en-US / ja-JP copy. |
| Sidebar shows `Governance` instead of `Voting` | `complete` | Code is updated; no live renderer walk-through was executed. |

---

## Immediate Next Steps

FP-4 (code review finalization) is **complete** — this document is already finalized.

Remaining actionable gaps are scoped in the [Slice-1 Final Pass PRD](./slice-1-final-pass-PRD.md). The priority order is:

1. **FP-1: Fix CLI network flag** — blocker; no queries succeed without `--mainnet` / `--testnet-magic`.
2. **FP-2: Fix structured error loss across IPC** — major; renderer shows empty errors on every failure. Primary fix: render `error.details` in the DRepDirectory UI.
3. **FP-3: Fix duplicate history push warning** — minor; Governance.tsx console noise (sub-tab + sidebar).
4. **FP-9: Rename `DRepStatus` `expired` → `inactive`** — minor; ledger/design vocabulary alignment (see Final Pass PRD D4).
5. **FP-5–FP-6: Documentation/bookkeeping verification** — verify task statuses, evaluate `origin` property.

The long-running gaps (real synced-node fixtures, repository-wide lint/prettier) remain documented in this review as partial/open items but are deferred past the Final Pass. The `active`/`inactive` semantics question is now resolved (FP-9); `expiring`/`retired` are deferred to their owning slices.