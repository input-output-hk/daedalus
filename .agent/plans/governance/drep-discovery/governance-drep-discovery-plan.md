# Governance DRep Discovery Plan

## Overview

This plan covers the full DRep Discovery flow required for Daedalus users to discover, evaluate, favorite, select, and delegate to a DRep without leaving the app.

The plan remains local-first: DRep discovery data comes from the Daedalus-managed local Cardano node and cardano-wallet only. Verified DRep profile metadata may be rendered as secondary off-chain content only after the anchor bytes are fetched with hardened transport rules, hash-verified against the on-chain anchor hash, parsed, and clearly labeled.

## Task Tracking

- Companion tasks JSON: [governance-drep-discovery-plan-tasks.json](governance-drep-discovery-plan-tasks.json)

---

## Plan Boundary

This document and its companion task tracker are the canonical DRep Discovery implementation plan under `.agent/plans/governance/`. The plan owns the complete in-app DRep Discovery flow: browse, evaluate, favorite, select, review confirmation, and delegate through the existing Daedalus delegation paths.

The following surfaces are outside this plan unless they are explicitly added to this document and task tracker:

- Governance explorer surfaces for constitution, committee, proposals, treasury, and proposal details.
- Governance dashboard surfaces for active proposal voting, delegation overview, and DRep performance.
- Anchor-pipeline generalisation for constitution, governance action rationale, and constitutional committee rationale.
- DRep registration and active proposal voting.
- Local governance history.
- An "Enhanced / Explorer" power-user view of the DRep directory (filters always visible, persistent detail pane, comparison-friendly layout). Flagged by initial stakeholder feedback as a desirable Phase-2 enhancement, behind a toggle from the default view, once the Phase-1 browse → detail → delegate flow is stable.

Artifact placement decision: this plan and its task tracker are canonical under `.agent/plans/governance/` only.

---

## Goals

- Replace the GovTool-only discovery path with in-app DRep discovery backed by local Cardano state.
- Let users browse an unbiased default DRep cohort, search or show all DReps, inspect DRep details, and favorite DReps.
- Let users select a DRep from the discovery surface and delegate through the existing software-wallet and hardware-wallet flows.
- Render verified DRep anchor metadata only after hardened fetch, hash verification, immutable hash-keyed caching, parsing, and source labeling are complete.
- Preserve current-state-only behavior until a separate local-history initiative is approved.
- Tell users the current governance vote (DRep / Abstain / No Confidence) held by the selected wallet and pre-fill the existing `VotingPowerDelegation` flow from that state.
- Sanitize DRep ids and `abstain` / `no_confidence` literals out of logs and analytics before plumbing the current-vote state into the renderer.
- When a wallet has not delegated, surface the CIP-1694 reward-withdrawal restriction and nudge the user to choose a DRep, Abstain, or No Confidence — Daedalus never picks a delegation automatically.
- Display the DRep's `body.givenName` (CIP-119) inline alongside the DRep id and offer two links: the in-app DRep detail view and the raw anchor URL.

## Non-Goals

- No hosted explorers, hosted indexers, GovTool, Koios, Blockfrost, or public governance APIs as application data sources.
- No new delegation backend. The selector only supplies a DRep ID to the existing delegation flow.
- No proposal explorer, constitution explorer, treasury explorer, or committee explorer work.
- No DRep registration, DRep retirement, or proposal voting write flows.
- No historical metadata or previous-governance-state browsing.
- No IPFS transport implementation in this release; keep the transport interface slot reserved for a later approved phase.

---

## Requirements

- [ ] Use local `cardano-node` and `cardano-wallet` as the only sources of truth for DRep discovery and delegation status.
- [ ] Use a main-process `cardano-cli` wrapper for DRep ledger-state reads, with the renderer receiving typed payloads only through IPC.
- [ ] Capture representative `drep-state`, `drep-stake-distribution`, and `gov-state` fixtures before merging DRep query-service implementation.
- [ ] Own and expose the cardano-node socket path across the launcher lifecycle before the query service is added.
- [ ] Preserve lovelace precision end to end: parse CLI stdout with a token-preserving JSON parser, represent voting power as `BigNumber | null`, serialize IPC lovelace fields as decimal strings, and rehydrate in the renderer.
- [ ] Default directory view excludes the top 35 DReps by voting power, then shows up to the next 200 eligible DReps in randomized order. This default IS the "Recommended" sort for the first release; no separate Recommended tab or per-card Recommended badge is introduced.
- [ ] Eligible default-cohort DReps must be active, have remaining `drepActivity` of more than 6 epochs, and have completed DRep anchor metadata once verified anchor metadata is available. Mock screenshots showing "Expiring in 3 epochs" entries inside the default cohort are fixture-only and MUST NOT ship; production renders must respect the 6-epoch floor.
- [ ] Each DRep card in the directory and detail surfaces an informational category badge with a tooltip explaining why the DRep is in that category. Slice-5 ships Primary / Threshold / Non-metadata; High value activates in anchor-1 once verified metadata is available.
- [ ] The excluded top 35 and non-cohort DReps remain reachable through search/show-all filters and direct DRep ID entry.
- [ ] DRep discovery surfaces load the latest local state on entry and provide an explicit refresh control.
- [ ] DRep favorites persist across app sessions through Electron local store and are not per-wallet or synced.
- [ ] Slice 4 local discovery renders local on-chain fields and anchor presence only; it does not render anchor-derived profile content.
- [ ] Verified anchor metadata appears only after hash verification and source labeling are in place.
- [ ] All anchor-derived content carries a verified off-chain content source label wherever it is rendered.
- [ ] DRep selector state crosses into the delegation form through React Router `location.state`. `VotingStore` must not read `GovernanceStore` directly.
- [ ] Delegation submission continues through the existing software-wallet `delegateVotes` request and existing hardware-wallet signing path in `VotingStore`.
- [ ] Slice 2 confirmation shows the DRep ID only; verified display names are added only after the verified anchor pipeline is active.
- [ ] All user-visible DRep Discovery text ships with polished en-US and ja-JP copy.
- [ ] Widen `WalletDelegation` / `WalletNextDelegation` with `voting?: WalletVotingTarget` (discriminated by `kind`) carrying a canonical `DRepIdentity` (`raw`, `cip129?`, `cip105?`, `credentialHex?`, `credentialType: 'key' | 'script'`).
- [ ] Fix the latent literal mismatch where Daedalus writes `'voting_and_delegating'` but the cardano-wallet wire emits `'delegating_and_voting'`, preserving the public `WalletDelegationStatuses.VOTING_AND_DELEGATING` export name.
- [ ] Sanitize `filterLogData` for `dRepId`, `vote`, `voting` keys and reduce the `Casted governance vote` analytics payload to `voteKind` only.
- [ ] Render a `CurrentVoteSummary` panel above the `VotingPowerDelegation` form. The panel ALWAYS renders; states are: `noDelegation` (warning + nudge), `drep` (givenName + source label + id + two links: in-app view details, external anchor URL), `abstain`, and `no_confidence`. The `drep` state also renders the delegated DRep's current active/inactive/expiring status badge so the user can tell at a glance whether their existing delegation is still effective. Pre-fill form state from the current on-chain delegation (or leave empty for `noDelegation`); disable submit when the form selection matches the current delegation after canonical normalization.
- [ ] Add a five-value `currentVote` Storybook knob (`noDelegation | drepVerified | drepUnverified | abstain | noConfidence`) to every governance story via a pure wallet factory and `key`-based remount. Default value is `noDelegation` so the warning + nudge state is visible on first load.
- [ ] Surface a reward-withdrawal warning in the `noDelegation` state using copy grounded in CIP-1694 (rewards blocked from withdrawal until the stake credential delegates to a DRep, Abstain, or No Confidence). Daedalus must not auto-select a delegation.
- [ ] When the displayed delegation has an anchor, render the `body.givenName` field (CIP-119) and link both to (a) the in-app DRep detail route and (b) the raw anchor URL in a new browser context (`target="_blank" rel="noopener noreferrer"`).
- [ ] Use the CIP-119 test vectors as fixture provenance in stories and tests: SIPO mainnet (`https://sipo.tokyo/drep/SIPO.jsonld`), Cardano Academy preprod (`https://raw.githubusercontent.com/cardano-foundation/cardano-academy/refs/heads/main/Cardano%20Academy.jsonld`), and the canonical CIP-119 example (`https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/examples/drep.jsonld`).

---

## Acceptance Criteria

- Users can open an in-app DRep directory without visiting an external portal.
- Users can refresh DRep discovery data to the latest local state.
- Users can browse the default cohort, inspect why entries are included or excluded, and use search/show-all for all other DReps.
- Users can view DRep detail with local on-chain fields: DRep ID, registration state, active/inactive state, expiry, voting power or delegated stake, current epoch vote positions on active proposals, and anchor presence.
- Users can view verified DRep profile metadata after anchor verification succeeds, with clear source labeling and graceful absence or failure states.
- Users can favorite and unfavorite DReps, and favorites persist across app restart.
- Users can select a DRep from discovery, see the selected DRep ID in the delegation form, review it in confirmation, and submit through the existing delegation path.
- Hardware-wallet users keep the existing signing path, including on-device confirmation and failure handling.
- Query and parse failures surface as typed payloads rather than partial or misleading directory results.
- Users can complete browse -> evaluate -> select -> delegate without external portals on a synced node.
- Users see the current governance delegation (DRep id / Abstain / No Confidence) of the selected wallet without leaving the Governance page.
- Users cannot submit a vote identical to the current on-chain delegation; the server-side `same_vote` error remains the safety net.
- DRep ids, `abstain` / `no_confidence` literals, and CIP-129/CIP-105 strings appear in the renderer DOM only — never in logs, electron-store, or analytics — verified by automated spies.
- Users whose wallet has no governance delegation see a warning that staking rewards cannot be withdrawn until they delegate, and a clear CTA to choose a delegation.
- Users see the DRep's display name (CIP-119 `body.givenName`) and can open both the in-app DRep detail view and the raw anchor URL from the panel.

---

## Key Decisions

| Decision | Resolution |
|---|---|
| Scope | Full DRep Discovery flow: browse, evaluate, favorite, select, confirmation, and delegation through existing paths. |
| Artifact placement | Canonical `.agent/plans/governance/` files only. |
| Navigation | Rename the existing `Voting` sidebar entry to `Governance`; governance delegation, DRep discovery, and Catalyst voting sit under that single entry. |
| Data source | Local `cardano-node` and `cardano-wallet` only. |
| Query mechanism | Main-process `cardano-cli` wrapper against the cardano-launcher-managed local node IPC endpoint. |
| Era flag | Prefer `cardano-cli latest query ...`; fall back to `conway` only if the bundled CLI lacks `latest`. |
| Socket path | Configure `nodeConfig.socketFile` pre-launch and read the launcher-resolved socket path after `node.start()`. CLI subcommands receive it through `CARDANO_NODE_SOCKET_PATH` in `spawn.env`. |
| DRep query shape | Bulk `--all-dreps` once per latest-state refresh. Per-DRep CLI invocations are forbidden for directory/detail refresh. |
| Lovelace precision | `BigNumber | null` in app models; decimal-string serialization across IPC; lossless CLI JSON parsing with `json-bigint`. |
| DRep selector handoff | Pass `selectedDRepId` through React Router `location.state`. |
| Default cohort | Exclude top 35 by voting power, then show up to the next 200 eligible DReps in randomized order. The "200 with top-35 excluded" sizing follows the Beyond MVG (BMVG) Simplified one-click-delegation analysis (Phase-1 variant, BMVG 2026-05-19); product copy and documentation cite BMVG when explaining the default. |
| Randomization seed | Reseed once per app session; explicit Reshuffle manually reseeds without a re-query. |
| Recommended framing | The default cohort IS the "Recommended" sort for this release. No separate Recommended tab, no per-card Recommended badge. The four-category badge set (High value / Primary / Threshold / Non-metadata) acts as the early per-DRep explanation surface and lays the groundwork for a future Recommended badge / section. |
| High Value badge | Deferred to anchor-1. Slice-5 ships Primary / Threshold / Non-metadata only. |
| DRep category badges | Each DRep card and the detail view render exactly one informational category badge (High value / Primary / Threshold / Non-metadata) with a tooltip explaining the rule that placed the DRep there. Badges are visual annotation only; they never reorder the cohort or override the randomized default. |
| Anchor content | Render only after hardened fetch, Blake2b-256 hash verification, immutable hash-keyed caching, parsing, and source labeling. |
| Delegation boundary | No second delegation backend. Selection only supplies a DRep ID to existing software-wallet and hardware-wallet paths. |
| Current-state boundary | No historical governance or stale anchor-content views in this release. |
| Favorites scope | Per-device via Electron local store. Not per-wallet, not synced. Wallet restore on a new machine does not carry favorites. |
| Wire status literal | Wire value is `delegating_and_voting`; the Daedalus `'voting_and_delegating'` literal is a bug fixed in cv-1 (task-128) (constant export name preserved). |
| `ApiDRep` discrimination | Bech32 HRP of raw on-wire string: `"abstain"` / `"no_confidence"` sentinels; `drep1…` (CIP-129); `drep_vkh1…` / `drep_script1…` (CIP-105). |
| Delegation effective semantics | Per CIP-1694, vote delegation has no per-action waiting period — the newest on-chain delegation IS the current delegation. The renderer uses `delegation.active.voting` as the authoritative current state. Historical vote-delegation browsing is out of scope for v1. |
| Same-vote prevention | Client-side after canonical-form normalization; server `same_vote` error retained as authoritative safety net. |
| Storybook isolation | Each `currentVote` knob change uses a pure wallet factory and force-remounts `VotingPowerDelegation` via a React `key`. No mutation of module-level `GOVERNANCE_WALLETS`. |
| No auto-delegation | Daedalus never sets a default DRep. When a wallet has not delegated, the panel must show the CIP-1694 reward-withdrawal warning and a CTA to choose one. |
| DRep name source | CIP-119 `body.givenName` only. If the anchor has not been verified, the name is hidden and only the DRep id is shown with an unverified source label. |
| Anchor URL display | Show as an external link with `target="_blank" rel="noopener noreferrer"`. Never render the raw anchor JSON inline in the panel; the DRep detail view owns full anchor rendering. |

---

## Discovery Query Findings

The initial query-path gate has been folded into this plan as research findings rather than a separate implementation sprint.

- Production DRep discovery reads should use a singleton Electron main-process `cardano-cli` wrapper against the cardano-launcher-managed local node. The renderer receives typed IPC payloads only.
- `cardano-wallet` remains a wallet-scoped delegation/status source for this release, not a DRep directory source. The pinned `cardano-wallet` swagger for `v2026-05-11` / `c642e0779676d2567e3d5fa1e2db9f029b6398e1` exposes `PUT /v2/dreps/{drepId}/wallets/{walletId}` with `operationId: joinDRep`; no directory-wide DRep read endpoint was found.
- An Ogmios sidecar is rejected for this release because it adds another managed service, package surface, IPC/security boundary, and synchronization path while the Daedalus-managed local node can answer the required ledger reads through `cardano-cli`.
- The current flake pins `cardano-node` / `cardano-cli` to `github:IntersectMBO/cardano-node/11.0.1` at `97036a66bcf8c89f687ae57a048eecc0389977ef`, NAR hash `sha256-+dod+EL73MICgbgflyJnwDlxbCeaBbBLrr2tLX59hAA=`, and `cardano-wallet` to `github:cardano-foundation/cardano-wallet/v2026-05-11` at `c642e0779676d2567e3d5fa1e2db9f029b6398e1`, NAR hash `sha256-8Gp5/vYh01EVZ3++wCBfLOq+9roPtswQITHIsvDUGUE=`.
- The root `cardano-cli`, `cardano-node`, and `cardano-wallet` symlinks in this checkout currently resolve to an older `daedalus-cardano-bridge` output (`version` file `2025.3.31`). Refresh the Nix dev shell or bridge output before producing release fixtures.
- The locally available CLI still confirms the required command shape: `latest` and `conway` era-prefixed commands are available, `latest query` lists `drep-state`, `drep-stake-distribution`, and `gov-state`, and the DRep queries support `--all-dreps` plus `--output-json`.
- `cardano-cli latest query` documents `CARDANO_NODE_SOCKET_PATH` as the local-node socket source, and the specific query commands make `--socket-path` optional when that environment variable is set. DRep Discovery subprocesses should therefore set the socket in `spawn.env`, not pass it as user-controllable argv.
- `cardano-launcher` already supports `nodeConfig.socketFile` and exposes the launcher-resolved `node.nodeService.socketPath`; Daedalus currently does not set `socketFile` before launcher construction or expose the resolved path from `CardanoNode`, so slice-1 owns that prerequisite.
- No new package or flake input is required for the plan revision: `cardano-cli` comes from the `cardano-node` flake input, `bignumber.js`, `json-bigint`, `blake2b`, `blakejs`, `cardano-launcher`, `electron-store`, and Node HTTPS/DNS primitives are already present.
- Live DRep JSON fixture capture could not be completed in this session: `CARDANO_NODE_SOCKET_PATH` was unset, no `cardano-node` process was running, and the only discovered preview socket refused connection. The query-service merge remains blocked until synced-node fixtures for `drep-state`, `drep-stake-distribution`, and `gov-state` are committed under `tests/mocks/governance/`, including at least one lovelace value above `Number.MAX_SAFE_INTEGER`.

---

## Current Baseline

- Daedalus already supports software-wallet governance delegation through `PUT /v2/dreps/{dRepId}/wallets/{walletId}` in [source/renderer/app/api/voting/requests/delegateVotes.ts](../../../source/renderer/app/api/voting/requests/delegateVotes.ts).
- The hardware-wallet governance path already builds a `cast_vote` certificate in [source/renderer/app/stores/VotingStore.ts](../../../source/renderer/app/stores/VotingStore.ts).
- The existing voting governance route is mounted from [source/renderer/app/Routes.tsx](../../../source/renderer/app/Routes.tsx) and route literals live in [source/renderer/app/routes-config.ts](../../../source/renderer/app/routes-config.ts).
- Daedalus currently routes users externally for DRep discovery via localized GovTool-oriented copy in [source/renderer/app/i18n/locales/en-US.json](../../../source/renderer/app/i18n/locales/en-US.json) and [source/renderer/app/i18n/locales/ja-JP.json](../../../source/renderer/app/i18n/locales/ja-JP.json).
- No `GovernanceStore`, governance read IPC channels, main-process DRep query service, or in-app DRep directory exists yet.

## Toolchain Baseline

- `cardano-wallet`: `github:cardano-foundation/cardano-wallet/v2026-05-11` in [flake.nix](../../../flake.nix)
- `cardano-node` / `cardano-cli`: `github:IntersectMBO/cardano-node/11.0.1` in [flake.nix](../../../flake.nix)
- Current flake lock references are recorded in [flake.lock](../../../flake.lock); refresh local root tool symlinks through Nix before capturing DRep query fixtures.
- Runtime/build stack: Electron 41.3.0, React 16.14.0, MobX 5.15.7, TypeScript 4.9.5, Webpack 5.106.2, Node `>=v22.0.0`, Yarn 1.22.x in [package.json](../../../package.json)
- Already-present supporting packages: `bignumber.js@9.0.1`, `json-bigint@1.0.0`, `blake2b@2.1.3`, `blakejs@1.1.0`, and `cardano-launcher@0.20220119.0`

---

## Technical Design

### Main-Process Query Service

- Add a singleton `GovernanceQueryService` in the Electron main process.
- Use the confirmed `cardano-cli` path and call `latest query drep-state --all-dreps --output-json`, `latest query drep-stake-distribution --all-dreps --output-json`, and `latest query gov-state --output-json`.
- Read the captured node socket path from the running `CardanoNode` instance and pass it through `CARDANO_NODE_SOCKET_PATH` in `child_process.spawn` environment.
- Deduplicate in-flight refreshes and retain last-successful data only for stale-while-refresh continuity.
- Return typed failures for whole-call query errors, ranking-unavailable states, invalid DRep IDs, parse failures, and selfnode capability errors.
- Preserve large lovelace values by parsing CLI stdout with `json-bigint` in lossless mode and converting raw decimal strings into `BigNumber`.

### Shared Types And IPC

- Add shared DRep discovery types under `source/common/types/`.
- Add governance read IPC contracts in `source/common/ipc/api.ts` for DRep list, DRep detail, wallet governance status, and anchor metadata payloads when available.
- Register main-process handlers in `source/main/ipc/index.ts`.
- Add renderer IPC clients under `source/renderer/app/ipc/`, following the existing main/renderer split pattern.
- Document IPC serialization for all lovelace fields as decimal strings.

### Renderer State

- Add `GovernanceStore` and register it in [source/renderer/app/stores/index.ts](../../../source/renderer/app/stores/index.ts).
- Own DRep list, browsing detail, favorites, selected delegation target, default cohort state, search/show-all state, refresh metadata, loading states, and typed errors.
- Keep the browsing detail separate from the delegation target.
- Persist favorites through Electron local store, per device, not per wallet.
- Keep `VotingStore` independent from `GovernanceStore`; selected DRep ID crosses the boundary through React Router `location.state`.

### Discovery UX

- Add DRep directory and detail surfaces under `source/renderer/app/components/`.
- Directory columns include DRep ID, active/inactive state, expiry status, voting power, metadata status, and favorite toggle.
- Filters include active status, expiry threshold, metadata status, top-35 exclusion, default cohort, favorited, search, and show-all.
- Detail view shows local on-chain fields first and remains useful when anchor metadata is unavailable.
- Add routes for DRep directory and DRep detail if the implementation uses separate routed pages.
- Integrate a Browse DReps affordance into [source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx](../../../source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx).
- Retain direct DRep ID paste entry.

The committed information architecture renames the existing `Voting` sidebar entry to `Governance` and nests the governance delegation form, DRep directory/detail/favorites, and Catalyst voting beneath that single entry. It avoids a second top-level governance item while still absorbing the future governance surfaces flagged in the Plan Boundary (proposals, constitution, committee, treasury, dashboard). The largest early routing cost (layout container, navigation updates, and additional sub-routes) remains the acknowledged trade-off. See the [design folder README](./README.md) and [drep-discovery-design.md](./designs/drep-discovery-design.md) for full rationale; shared tokens, copy, and component contracts live in [shared-design-tokens.md](./designs/shared-design-tokens.md).

### Anchor Metadata Pipeline

- Add a transport-agnostic main-process anchor fetch service with HTTP/HTTPS implemented and an IPFS slot reserved.
- Fetch only bounded raw bytes; do not parse JSON or write cache entries during raw fetch.
- Keep TLS verification enabled, disable redirects by default, enforce connect+total timeout <= 10 seconds, enforce about a 1 MB response cap, and allow only JSON content types.
- Add SSRF protection with DNS-rebinding mitigation by binding the validated resolved IP to the actual connection.
- Verify Blake2b-256 hash against the on-chain anchor hash before parsing, caching, or rendering.
- Cache only verified content in an immutable local cache keyed by anchor hash.
- Do not serve stale cached content when the on-chain anchor hash changes.

### Delegation Integration

- Selecting a DRep pre-fills the existing delegation form.
- Confirmation shows DRep ID in the local-only milestone.
- Confirmation may show a verified display name only after the verified anchor pipeline and source labeling are active.
- Software-wallet submission continues through the existing `delegateVotes` request.
- Hardware-wallet submission continues through the existing initialization, signing, and confirmation flow in `VotingStore`.

---

## Vertical Slice Sequencing

> **Note on sequencing.** This plan is sequenced as vertical capability slices, not horizontal layers. The companion [task tracker](governance-drep-discovery-plan-tasks.json) is the source of truth: 58 tasks across 13 phases (`slice-1`..`slice-8`, `cv-1`, `cv-2`, `anchor-1`, `anchor-2`, `standing`).

### Slice definition of done

Every slice is a thin, demoable, end-to-end capability:

- It is demoable behind **real IPC** — no slice is pure-backend or pure-UI.
- It lands its **own** Storybook, Jest, Cucumber, and i18n coverage in-slice. There is no terminal test phase and no terminal i18n phase.
- Later slices **inherit a no-leak acceptance check**: no DRep id / `abstain` / `no_confidence` / CIP-129 / CIP-105 string appears in any logger or analytics payload.

### Foundation slices (sequential)

1. **Slice 1 — Walking skeleton + sanitization floor.** Real node socket path, real `cardano-cli --all-dreps`, lossless `json-bigint` parse, decimal-string IPC, `BigNumber` rehydrate, `GovernanceStore` with `drepIndex`, a bare directory list (id / voting power / active-inactive), the directory route inside the renamed Governance section, and the log/analytics sanitization. This is the first demoable slice.
2. **Slice 2 — Software-wallet delegate.** Closes the primary journey: select a directory row → pre-fill `VotingPowerDelegation` → confirm DRep-ID-only → submit via the existing software `delegateVotes` path. Drops the false detail-view dependency that previously blocked select→delegate.
3. **Slice 3 — Hardware-wallet delegate.** The HW delegate path with DRep-ID-only confirmation, in-slice Cucumber with mocked Ledger/Trezor and on-device byte-equality (`vote.chosenOption` equals the selected DRep id).

### Parallelizable tracks (after the foundation)

- **Track D (discovery thickening, evaluation-quality first):** `slice-4` detail view (on-chain only) → `slice-5` default cohort + category badges + BMVG banner → `slice-6` search / show-all → `slice-7` favorites → `slice-8` refresh-latency + selfnode + release verification.
- **Track V (current vote):** `cv-1` voting-field plumbing + the `CurrentVoteSummary` core states (noDelegation / abstain / no_confidence / DRep-ID-only) → `cv-2` live status badge from the directory `drepIndex` + pre-fill + same-vote prevention + e2e.
- **Track A (anchor enrichment):** `anchor-1` hardened fetch + hash-verify + immutable cache + first verified `givenName` render → `anchor-2` remaining CIP-119 profile fields + `doNotList` + confirmation identity migration + source-label sweep.

### Non-negotiable floors (never thinned across slices)

- **Log / analytics sanitization** lands in `slice-1` and is re-asserted as an inherited acceptance check in every later slice.
- **Anchor transport-security guards** (SSRF, redirect, TLS, size, timeout, content-type, DNS-rebinding) are completed in full in `anchor-1` and are never thinned in `anchor-2`.

### Key dependency facts

- **Select → delegate does NOT depend on the detail view.** The primary journey closes in `slice-2` before any detail / cohort / search work.
- **The `cv-2` live status badge depends on the directory `drepIndex`** populated by `slice-1`; the `cv-1` core states (noDelegation / abstain / no_confidence / DRep-ID-only) do not.
- **`anchor-1` depends on the detail view** (`slice-4`) as its first verified render surface.
- **`anchor-2` `doNotList` exclusion depends on the cohort rule** (`slice-5`).

---

## Testing Strategy

- Run `yarn compile`, `yarn lint`, `yarn prettier:check`, `yarn i18n:manage`, and `yarn storybook:build` for release readiness.
- Add Jest coverage for shared DRep types, lossless parsing, query-service behavior, latest-on-load and refresh behavior, default cohort ranking/randomization, anchor fetch and verification, immutable cache behavior, `GovernanceStore`, selector integration, confirmation identity display, and hardware-wallet propagation.
- Add Cucumber coverage for DRep directory browsing, refresh, search/show-all, DRep detail, verified anchor profile display, favorite persistence, selector handoff, software-wallet delegation, and mocked Ledger/Trezor delegation paths.
- Add Storybook stories for directory, detail, selector, favorite toggle, source labels, confirmation dialog states, and loading/error/empty states.
- Perform manual release validation on a synced node for the complete browse -> evaluate -> select -> delegate flow.
- For Track V (cv-1 / cv-2): cover `_createWalletFromServerData` voting mapping, `Wallet.currentVote` / `isVoting` computeds, `CurrentVoteSummary` component snapshots per knob value, the Cucumber `@e2e` governance-current-vote feature, and the sanitization regression spying over `logger.*` and `AnalyticsTracker.sendEvent`.

---

## Rollout Plan

- Synced-node fixtures for `drep-state`, `drep-stake-distribution`, and `gov-state` must be captured from a synced node and committed under `tests/mocks/governance/` before `task-103` merges.
- `cardano-launcher@0.20220119.0` `socketFile` support must be verified before `task-102` starts; if the pinned launcher cannot supply it, stop and reopen the socket-path ownership approach.
- Keep slice-4 detail local-on-chain only; do not render anchor-derived metadata before fetch, verification, cache, parse, and source-labeling hardening land.
- Use the anchor-display feature flag for staged verification control, not as a permanent production off-switch.
- Release DRep Discovery only when users can complete the full in-app discovery and delegation flow without external portals.
- Treat the slice-1 sanitization floor (task-109, task-110, and task-111) as a hard prerequisite for any UI work that surfaces `delegation.active.voting`; no current-vote rendering ships until those land.

---

## Risks And Mitigations

| Risk | Mitigation |
|---|---|
| Node socket path is unavailable to CLI queries | Own `nodeConfig.socketFile` before launcher construction and capture the launcher-resolved path after `node.start()`. |
| Lovelace precision loss corrupts ranking | Use token-preserving JSON parsing, `BigNumber`, and decimal-string IPC serialization. |
| Query process count scales with DRep count | Use bulk `--all-dreps` queries only; forbid per-DRep CLI invocations for directory/detail refresh. |
| Ranking unavailable from stake-distribution failure | Surface typed ranking-unavailable failure instead of silently falling back to biased ordering. |
| Anchor fetch creates SSRF or TOCTOU exposure | Enforce hardened HTTP/HTTPS transport guards and DNS-rebinding mitigation. |
| Unverified metadata misleads delegation | Render anchor-derived content only after hash verification and label it as verified off-chain content. |
| Selector creates a competing delegation path | Keep selection as DRep ID handoff only; reuse existing `delegateVotes` and hardware-wallet signing paths. |
| `doNotList` DReps remain visible in the default cohort until anchor-2 lands | Accepted as a known interim state from slice-5 to anchor-2. `doNotList` exclusion is not security-critical and becomes enforceable once verified metadata is available. |
| Wallet API surface changes invalidate query rationale | Keep the per-bump swagger-grep checklist and re-open the decision if wallet adds directory-wide DRep endpoints. |
| Latent `voting_and_delegating` literal blocks all VOTING_AND_DELEGATING resolution | cv-1 (task-128) fixes the underlying string while preserving the export name and adds a unit test asserting the constant equals the wire literal. |
| Vote target leaks into logs or analytics | The slice-1 sanitization floor (task-109 + task-110) widens `filterLogData` and reduces the `Casted governance vote` payload to vote-kind only, with a Jest spy regression test in task-111. |
| Pre-fill from stale `Wallet` instance silently drops vote target after polling refresh | cv-1 (task-131) adds `votingTarget` to the `Wallet.update()` pick list and derives the selected wallet from `selectedWalletId` against the latest snapshot. |
| Misframing vote-delegation timing as stake-pool-delegation timing | The renderer uses CIP-1694 liquid-democracy framing: the newest on-chain vote delegation IS current. Historical vote-delegation browsing remains out of scope for v1. |
| User assumes Daedalus auto-delegates | The `noDelegation` state explicitly states Daedalus will not pick a DRep and presents a CTA. Confirmed by code review: `delegateVotes` only fires from explicit form submission. |
| Anchor URL leads to malicious content | Harden `open-external-url.ts` with an HTTPS-only scheme allow-list before any anchor URL is rendered. Reject `javascript:`, `file:`, `data:`, and other non-HTTPS schemes; full anchor parsing/validation still happens in the hardened anchor pipeline (anchor-1), never inline in the current-vote panel. |
| `json-bigint` lossless-mode objects are not plain JS and may not survive structured-clone across IPC or MobX observability | Convert lossless values to decimal strings before they cross IPC (per the lovelace serialization rule) and rehydrate to `BigNumber` in the renderer; never pass raw `JSONbig` objects through `ipcRenderer.invoke` or into observable state. |
| Anchor 1 MB response cap can reject valid CIP-119 metadata that inlines a base64 `imageObject` | Confirm the cap against realistic CIP-119 payloads with inline images; either raise the cap for the image case or document that inline-image DReps degrade to the default avatar with an explicit source label. |
| `cardano-launcher@0.20220119.0` is the pinned launcher; `nodeConfig.socketFile` support (relied on by task-102) is unverified against this version | Verify `socketFile` support in the pinned launcher version before starting slice-1; if unsupported, escalate the socket-path ownership approach. |

---

## Open Questions



---

## References

- Companion task tracker: [governance-drep-discovery-plan-tasks.json](governance-drep-discovery-plan-tasks.json)
- Current wallet delegation request: [source/renderer/app/api/voting/requests/delegateVotes.ts](../../../source/renderer/app/api/voting/requests/delegateVotes.ts)
- Current voting delegation component: [source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx](../../../source/renderer/app/components/voting/voting-governance/VotingPowerDelegation.tsx)
- Current hardware governance certificate flow: [source/renderer/app/stores/VotingStore.ts](../../../source/renderer/app/stores/VotingStore.ts)
- CIP-119: https://cips.cardano.org/cip/CIP-0119
- CIP-1694: https://cips.cardano.org/cip/CIP-1694

### UX Design Artifacts

- Design folder overview, rationale, and locked decisions: [README.md](./README.md)
- External research: [external-research.md](./research/external-research.md)
- Shared design tokens (status badges, source labels, formatting, refresh state, copy): [shared-design-tokens.md](./designs/shared-design-tokens.md)
- Dedicated Governance section design: [drep-discovery-design.md](./designs/drep-discovery-design.md)

### Wallet Current Vote Display

- Current Vote Display tech design: [current-vote-display-design.md](./designs/current-vote-display-design.md)
- Current Vote Display UX spec: [current-vote-display-ux.md](./designs/current-vote-display-ux.md)
- CIP-1694 (liquid democracy, vote delegation semantics, reward-withdrawal gate): https://github.com/cardano-foundation/CIPs/blob/master/CIP-1694/README.md
- CIP-119 test vector spec: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/test-vector.md
- CIP-119 canonical example (`drep.jsonld`, hash `a14a5ad4f36bddc00f92ddb39fd9ac633c0fd43f8bfa57758f9163d10ef916de`): https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/examples/drep.jsonld
- Mainnet test vector — SIPO: https://sipo.tokyo/drep/SIPO.jsonld
- Preprod test vector — Cardano Academy: https://raw.githubusercontent.com/cardano-foundation/cardano-academy/refs/heads/main/Cardano%20Academy.jsonld

---

## Changelog

Use the date of change `[year-month-day]` and a short description of the plan change.

### Added

### Changed

---

**Status:** In Progress
**Date:** 2026-06-03
**Author:** david-profrontsolutions (ft. GitHub Copilot)
