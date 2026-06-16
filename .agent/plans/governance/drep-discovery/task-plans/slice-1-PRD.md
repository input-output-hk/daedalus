# Slice-1 PRD: Walking Skeleton + Sanitization Floor

> **Status:** In Progress | **Date:** 2026-06-05 | **Parent Plan:** [governance-drep-discovery-plan.md](./governance-drep-discovery-plan.md)

---

## Executive Summary

Slice-1 establishes the thinnest possible end-to-end skeleton of the DRep Discovery feature: real `cardano-node` socket-path ownership, real `cardano-cli --all-dreps` queries, lossless `json-bigint` parsing, decimal-string IPC serialization, `BigNumber` rehydration in the renderer, and a bare directory list showing DRep ID, voting power, and active/inactive status on its own route. It also lands the log/analytics sanitization floor — no DRep ID, `abstain` / `no_confidence` literal, or CIP-129/CIP-105 string may ever appear in logger or analytics payloads from this slice onward.

This slice is the **first demoable artifact**: a real directory list populated from the user's local Cardano node, rendered in the renamed Governance section of the sidebar, with no external portal dependency.

---

## Problem Statement

Daedalus users currently have no in-app way to discover DReps. The governance delegation form only accepts a manually-pasted DRep ID, and all discovery is outsourced to external portals (GovTool). Users must leave the app, copy a DRep ID from a third-party tool, and paste it back — a disjointed, high-friction experience that undermines trust in the delegation process.

Slice-1 solves the **data plumbing** problem: getting DRep ledger state from the locally-managed Cardano node into the renderer UI through a secure, lossless, typed IPC pipeline. It does NOT yet solve discovery quality (cohorts, filters, favorites, detail views, anchor metadata) — those are added in later slices.

---

## User Stories

### US-1.1 — View DRep directory (bare)
**As a** Daedalus user with a synced node,  
**I want to** see a list of all DReps with their ID, voting power, and active/inactive status,  
**So that** I can confirm the pipeline works and see what DReps exist on-chain.

**Acceptance:**
- Directory loads automatically when the user navigates to the Governance section.
- Each row shows: DRep ID (truncated), voting power (₳-formatted from `BigNumber`), active/inactive status badge.
- Empty state renders when no DReps are available.
- Error state renders when the node socket is unavailable or the CLI query fails.
- Refresh control triggers a fresh `--all-dreps` query.

### US-1.2 — Lossless lovelace precision
**As a** developer,  
**I want to** ensure that large lovelace values are never truncated to JavaScript `Number`,  
**So that** voting power rankings are never silently corrupted.

**Acceptance:**
- CLI JSON output is parsed with `json-bigint` in lossless mode.
- All lovelace values are serialized as decimal strings across IPC.
- Renderer rehydrates strings into `BigNumber` instances.
- At least one test fixture contains a lovelace value above `Number.MAX_SAFE_INTEGER`.

### US-1.3 — Privacy: no vote targets in logs or analytics
**As a** privacy-conscious user,  
**I want to** ensure that my governance choices (which DRep I delegate to, or whether I abstain / vote no-confidence) are never written to log files or sent to analytics,  
**So that** my voting preferences remain private.

**Acceptance:**
- `filterLogData` redacts `dRepId`, `vote`, `voting` keys at any nesting depth.
- The `'Casted governance vote'` analytics event carries only `drepOption` (one of `'drep' | 'abstain' | 'no_confidence'`), never a raw DRep ID.
- Automated Jest spies assert that no CIP-129/CIP-105 bech32 string or `abstain`/`no_confidence` literal reaches any logger or analytics call.

---

## Functional Requirements

| ID | Requirement | Owner |
|----|------------|-------|
| FR-1 | `CardanoNode` must expose the launcher-resolved node socket path via a read-only accessor | task-102 |
| FR-2 | `GovernanceQueryService` must call `cardano-cli latest query drep-state --all-dreps --output-json` with `CARDANO_NODE_SOCKET_PATH` in the spawn environment | task-103 |
| FR-3 | CLI stdout must be parsed with `json-bigint` in lossless mode; all lovelace fields must be converted to decimal strings before IPC | task-103 |
| FR-4 | Shared DRep Discovery types must use `BigNumber \| null` for voting power and serialize as decimal strings across IPC | task-101 |
| FR-5 | Typed IPC channels for DRep list query must be defined in `source/common/ipc/api.ts` | task-104 |
| FR-6 | Main-process IPC handlers must be registered in `source/main/ipc/index.ts` | task-104 |
| FR-7 | Renderer IPC clients must be added under `source/renderer/app/ipc/` | task-105 |
| FR-8 | `GovernanceStore` must own DRep list (`drepIndex` keyed by DRep ID), loading state, refresh state, and typed error states | task-106 |
| FR-9 | Bare DRep directory list component must render DRep ID, voting power, and active/inactive status | task-107 |
| FR-10 | DRep directory route must be wired into `Routes.tsx` and `routes-config.ts` with the Voting sidebar entry renamed to Governance | task-108 |
| FR-11 | `filterLogData` must redact `dRepId`, `vote`, `voting` keys | task-109 |
| FR-12 | `'Casted governance vote'` analytics must carry only `drepOption`, not raw DRep ID | task-110 |
| FR-13 | Jest spy regression test must assert no vote targets leak to logs or analytics | task-111 |

---

## Non-Functional Requirements

| ID | Requirement |
|----|------------|
| NFR-1 | CLI subprocess must never receive `--socket-path` as user-controllable argv; only `CARDANO_NODE_SOCKET_PATH` in `spawn.env` |
| NFR-2 | Deduplicate in-flight refresh requests in `GovernanceQueryService` |
| NFR-3 | Retain last-successful data for stale-while-refresh continuity |
| NFR-4 | All user-visible text must ship with preliminary en-US and ja-JP copy via react-intl, keeping `!!!` at the front of each new string until a final manual review at the end of the full feature cycle |
| NFR-5 | `BigNumber` rehydration must happen in the renderer, never pass raw `JSONbig` objects through IPC |
| NFR-6 | The `Voting` sidebar entry rename to `Governance` must be reflected in the sidebar component and navigation labels |
| NFR-7 | Slice-1 renderer i18n work must stay within the existing `react-intl@2.9.0` API surface (`injectIntl`, `intlShape`, `FormattedMessage`) rather than hooks or `FormattedRelativeTime` |

---

## Architecture: Data Flow

```
┌──────────────┐    spawn(env)     ┌──────────────┐
│  CardanoNode  │ ──socketPath──→  │ Governance    │
│  (launcher)   │                  │ QueryService  │
└──────────────┘                  │ (singleton)   │
                                   │ json-bigint   │
                                   │ → decimal str │
                                   └──────┬───────┘
                                          │ typed IPC payload
                                   ┌──────▼───────┐
                                   │ IPC channel  │
                                   │ (main→rend)  │
                                   └──────┬───────┘
                                          │ decimal-string fields
                                   ┌──────▼───────┐
                                   │ Governance   │
                                   │ Store        │
                                   │ BigNumber    │
                                   │ rehydrate    │
                                   └──────┬───────┘
                                          │ observable
                                   ┌──────▼───────┐
                                   │ DRep         │
                                   │ Directory    │
                                   │ (bare list)  │
                                   └──────────────┘
```

---

## Route Changes

The existing `Voting` sidebar entry is renamed to `Governance`. New routes nest under `/governance`:

```
GOVERNANCE: {
  ROOT: '/governance',
  DREPS: '/governance/dreps',
}
```

`/governance` redirects to `/governance/dreps` (the Directory is the section landing page). The existing `/voting/governance` delegation route is preserved; the sidebar Governance entry highlights for both `/governance/*` and `/voting/*` paths.

---

## Component Tree (Slice-1)

```
Governance (new container, renamed from Voting)
├── GovernanceLayout (tab bar: Directory)
│   └── DRepDirectory (new)
│       ├── DRepDirectoryBanner (placeholder for cohort info, slice-5)
│       ├── DRepDirectoryList
│       │   └── DRepCard (bare: id + voting power + status badge)
│       └── DRepDirectoryEmpty (empty/error/loading states)
```

---

## What Slice-1 Deliberately Does NOT Include

- ❌ DRep detail view (slice-4)
- ❌ Default cohort / randomization / BMVG banner (slice-5)
- ❌ Category badges (slice-5)
- ❌ Search / show-all filters (slice-6)
- ❌ Favorite toggle (slice-7)
- ❌ DRep selection handoff to delegation form (slice-2)
- ❌ Anchor metadata fetch, verify, or render (anchor-1/2)
- ❌ Current vote display (cv-1/2)
- ❌ Hardware wallet delegation path (slice-3)
- ❌ Refresh latency budget / selfnode (slice-8)
- ❌ Per-DRep CLI invocations (forbidden forever; only bulk `--all-dreps`)

---

## Dependencies

| Depends On | Status |
|-----------|--------|
| `cardano-launcher@0.20220119.0` `socketFile` support | Must verify before task-102 |
| `cardano-cli` from flake `cardano-node/11.0.1` | Available via Nix shell |
| `json-bigint@1.0.0` | Already in `package.json` |
| `bignumber.js@9.0.1` | Already in `package.json` |
| `electron-store` | Already in `package.json` |
| Synced-node fixtures for `drep-state` | Must capture before task-103 merge |

---

## Risks Specific to Slice-1

| Risk | Mitigation |
|------|-----------|
| Socket path not exposed by launcher | Verify `cardano-launcher@0.20220119.0` `nodeConfig.socketFile` support before task-102 |
| `json-bigint` objects not plain JS | Convert to decimal strings before IPC; never pass raw `JSONbig` objects across boundary |
| CLI unavailable in selfnode mode | Return `SelfnodeCliUnsupported` typed error; render empty state with selfnode badge |
| No synced node for fixture capture | Use preprod testnet with `yarn nix:preprod` to capture fixtures |

---

## Definition of Done

- [ ] `yarn compile` passes with zero TypeScript errors
- [ ] `yarn lint` passes with zero ESLint errors
- [ ] `yarn prettier:check` passes
- [ ] DRep directory renders on `/governance/dreps` with real IPC data
- [ ] Loading, empty, and error states are reachable and render correctly
- [ ] Voting power displays in ₳-formatted `BigNumber` with no precision loss
- [ ] `filterLogData` redacts all governance vote keys
- [ ] `'Casted governance vote'` analytics carries `drepOption` only
- [ ] Jest spy test confirms zero leakage of DRep IDs / vote literals to logs or analytics
- [ ] Storybook stories exist for directory list (loaded / empty / error / refreshing states)
- [ ] en-US and ja-JP i18n keys exist for all user-visible text, and all new source copy remains prefixed with `!!!` pending the final manual review pass
- [ ] Sidebar shows "Governance" instead of "Voting"

---

## References

- Parent plan: [governance-drep-discovery-plan.md](./governance-drep-discovery-plan.md)
- Task tracker: [governance-drep-discovery-plan-tasks.json](./governance-drep-discovery-plan-tasks.json)
- Design: [drep-discovery-design.md](./designs/drep-discovery-design.md)
- Shared tokens: [shared-design-tokens.md](./designs/shared-design-tokens.md)
- Current vote display design: [current-vote-display-design.md](./designs/current-vote-display-design.md)
